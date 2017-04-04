package cs.cynth.rtl

sealed abstract class Expr {
  def size: Int

  def children: IndexedSeq[Expr]

  def check(): Unit = {
    children.foreach(_.check())
  }

  def pretty(): Unit = print(toString)
}

case class Literal(size: Int, value: Int) extends Expr {
  def children = IndexedSeq()

  override def toString = s"$size'd$value"
}

case class Ref(v: Variable) extends Expr {
  def children = IndexedSeq()

  def size: Int = v.size

  override def toString = s"${v.id}"
}

abstract class UnaryExpr extends Expr {
  def child: Expr

  def children = IndexedSeq(child)
}

abstract class BinaryExpr extends Expr {
  def left: Expr

  def right: Expr

  def children = IndexedSeq(left, right)

  override def check(): Unit = {
    super.check()
    assert(left.size == right.size)
  }
}

abstract class UnaryArithExpr extends UnaryExpr {
  def size: Int = child.size
}

abstract class BinaryArithExpr extends BinaryExpr {
  def size: Int = left.size
}

abstract class CompareExpr extends BinaryExpr {
  def size: Int = 1
}

case class AdditiveExpr(left: Expr, right: Expr, op: String) extends BinaryArithExpr {
  override def toString = s"($left) $op ($right)"
}

case class Neg(child: Expr) extends UnaryArithExpr {
  override def toString = "_ ($child)"
}

/*

case class ShiftLeft(child: Expr, dist: Int) extends UnaryArithExpr

case class ShiftRightLogical(child: Expr, dist: Int) extends UnaryArithExpr

case class ShiftRightArithmetic(child: Expr, dist: Int) extends UnaryArithExpr

case class Eq(left: Expr, right: Expr) extends CompareExpr

case class Ne(left: Expr, right: Expr) extends CompareExpr

// FIXME and, or, xor

case class BitNot(child: Expr) extends UnaryExpr {
  def size = child.size
}

case class Not(child: Expr) extends UnaryExpr {
  def size: Int = 1
} */

case class RelationalExpr(left: Expr, right: Expr, op: String, isSigned: Boolean) extends CompareExpr {
  override def toString = {
    val s = if (isSigned) "$signed" else ""
    s"$s($left) $op $s($right)"
  }
}

case class SignExtend(size: Int, child: Expr) extends UnaryExpr {
  override def check(): Unit = {
    super.check()
    assert(child.size < size)
  }

  override def toString = s"{${size - child.size}{$child[${child.size - 1}]}, $child}"
}

case class ZeroExtend(size: Int, child: Expr) extends UnaryExpr {
  override def check(): Unit = {
    super.check()
    assert(child.size < size)
  }

  override def toString = s"${size - child.size}'d0, ($child)}"
}

case class Truncate(size: Int, child: Expr) extends UnaryExpr {
  override def check(): Unit = {
    super.check()
    assert(size < child.size)
  }

  override def toString = s"($child)[${size - 1}:0]"
}

sealed abstract class Variable {
  def id: String

  def size: Int
}

class Local(val id: String, val size: Int) extends Variable

class Parameter(val id: String, val size: Int) extends Variable

object Statement {
  var counter: Int = 0

  def newId(): Int = {
    val id = counter
    counter += 1
    id
  }
}

sealed abstract class Statement {
  val id: Int = Statement.newId()

  def check(f: Function): Unit = {
    // nothing
  }

  def pretty(): Unit

  def emit(b: FunctionBody, next: Int): Unit = {
    println(s"        $id : begin")
    emitBody(b, next)
    println("        end")
  }

  def emitBody(b: FunctionBody, next: Int): Unit =
    throw new UnsupportedOperationException
}

class Assign(v: Variable, expr: Expr) extends Statement {
  override def check(f: Function): Unit = {
    super.check(f)
    assert(v.size == expr.size)
  }

  def pretty(): Unit = {
    print(s"  ${v.id} = ")
    expr.pretty()
    println(";")
  }

  override def emitBody(b: FunctionBody, next: Int): Unit = {
    println(s"          ${v.id} <= $expr;")
    println(s"          __state <= $next;")
  }

}

class Label(val id: String, var target: Target) {
  def this(id: String) = this(id, null)
}

class Goto(label: Label) extends Statement {
  override def emitBody(b: FunctionBody, next: Int): Unit = {
    println(s"          __state <= ${label.target.id};")
  }

  def pretty(): Unit = {
    println(s"  goto ${label.id};")
  }
}

class Target(label: Label) extends Statement {
  assert(label.target == null)
  label.target = this

  override def check(f: Function): Unit = {
    assert(label.target != null)
  }

  def pretty(): Unit = {
    println(s" ${label.id}:")
  }

  override def emitBody(b: FunctionBody, next: Int): Unit = {
    println(s"          __state <= $next;")
  }
}

class Branch(cond: Expr, thenLabel: Label, elseLabel: Label) extends Statement {
  override def check(f: Function): Unit = {
    super.check(f)
    assert(cond.size == 1)
  }

  def pretty(): Unit = {
    print("  if (")
    cond.pretty()
    println(")")
    println(s"    goto ${thenLabel.id};")
    println("  else")
    println(s"    goto ${elseLabel.id};")
  }

  override def emitBody(b: FunctionBody, next: Int): Unit = {
    println(s"          if ($cond)")
    println(s"            __state <= ${thenLabel.target.id}")
    println("          else")
    println(s"            __state <= ${elseLabel.target.id}")
  }
}

class Call(val target: Function,
           returnVariable: Option[Variable],
           arguments: IndexedSeq[Expr]) extends Statement {
  override def check(f: Function): Unit = {
    super.check(f)
    assert(target.parameters.length == arguments.length)
    (target.parameters, arguments).zipped.foreach { (p, a) =>
      assert(p.size == a.size)
    }
    assert(target.returnSize == returnVariable.map(_.size).getOrElse(0))
  }

  def pretty(): Unit = {
    print("  ")
    returnVariable.foreach { v =>
      print(s"${v.id} = ")
    }
    print(s"${target.id}(")
    arguments.zipWithIndex.foreach { case (a, i) =>
      a.pretty()
      if (i + 1 < arguments.length)
        print(", ")
    }
    println(");")
  }

  override def emit(b: FunctionBody, next: Int): Unit = {
    val validState = Statement.newId()

    println(s"        $id : begin")

    println(s"          if (__idle_${target.id}) begin")
    println(s"            __start_${target.id} <= 1;")

    (target.parameters, arguments).zipped.foreach { (p, a) =>
      println(s"            __p_${p.id}_${target.id} <= $a;")
    }

    println(s"            __state <= $validState;")
    println("          end")
    println("        end")

    println(s"        $validState : begin")
    println(s"          __start_${target.id} <= 0;")
    println(s"          if (__valid_${target.id}) begin")
    returnVariable.foreach { v =>
      println(s"            ${v.id} <= __retval_${target.id};")
    }
    println(s"            __state <= $next;")
    println(s"          end")
    println("        end")
  }
}

class Return(expr: Option[Expr]) extends Statement {
  override def check(f: Function): Unit = {
    assert(f.returnSize == expr.map(_.size).getOrElse(0))
  }

  def pretty(): Unit = {
    print("  return")
    expr.foreach { e =>
      print(" ")
      e.pretty()
    }
    println(";")
  }

  override def emitBody(b: FunctionBody, next: Int): Unit = {
    expr.foreach { e =>
      println(s"          __retval <= $e;")
    }

    println(s"          __state <= ${b.returnState};")
  }
}

case class FunctionBody(variables: IndexedSeq[Variable],
                        stmts: IndexedSeq[Statement]) {
  val idleState: Int = Statement.newId()
  val returnState: Int = Statement.newId()

  def initialState: Int =
    stmts.headOption.map(_.id).getOrElse(returnState)

  def check(f: Function): Unit = {
    stmts.foreach(_.check(f))
  }

  def emit(f: Function): Unit = {

    val targets: Set[Function] =
      stmts.flatMap {
        case c: Call =>
          Some(c.target)
        case _ => None
      }.toSet

    val externTargets = targets.filter(f => f.body.isEmpty)

    println(s"module ${f.id}(")
    println("  input __clk,")
    println("  input __resetn,")

    externTargets
      .foreach { f =>
        println("")
        println(s"  // ${f.id} interface")

        f.parameters.foreach { p =>
          println(s"  output reg [${p.size - 1}:0] __p_${p.id}_${f.id},")
        }

        if (f.returnSize > 0)
          println(s"  wire [${f.returnSize - 1}:0] __retval_${f.id},")

        println(s"  output reg __start_${f.id},")
        println(s"  input __idle_${f.id},")
        println(s"  input __valid_${f.id},")

        println("")
      }

    f.parameters.foreach { p =>
      println(s"  input [${p.size - 1}:0] __p_${p.id},")
    }

    if (f.returnSize > 0)
      println(s"  output [${f.returnSize - 1}:0] __retval,")

    println("  input __start,")
    println("  output __idle);")
    println("  output __valid);")

    println("  reg [31:0] __state;")

    variables.foreach { v =>
      println(s"  reg [${v.size - 1}:0] ${v.id};")
    }

    println(s"  assign __idle = (__state == $idleState);")
    println(s"  assign __valid = (__state == $returnState);")

    targets
      .filter(f => f.body.isDefined)
      .foreach(_.emitInstance(true))

    println("  always @(posedge clk) begin")
    println("    if (!resetn) begin")
    println(s"      __state <= $idleState;")
    println("    end else begin")
    println("      case (state)")
    println(s"        $idleState : begin")
    println("          if (__start)")
    println(s"            __state <= $initialState;")
    f.parameters.foreach { p =>
      println(s"            ${p.id} <= __p_${p.id};")
    }
    println("        end")

    stmts.zipWithIndex.foreach { case (s, i) =>
      val next =
        if (i + 1 < stmts.length)
          stmts(i + 1).id
        else
          returnState

      s.emit(this, next)
    }

    println(s"        $returnState : begin")
    println(s"          __state <= $idleState;")
    println("        end")

    println("      endcase")
    println("    end")
    println("  end")

    println("")

    println("endmodule")

    if (externTargets.nonEmpty) {
      println(s"module ${f.id}_top_template(")
      println("  input __clk,")
      println("  input __resetn,")

      f.parameters.foreach { p =>
        println(s"  input [${p.size - 1}:0] __p_${p.id},")
      }

      if (f.returnSize > 0)
        println(s"  output [${f.returnSize - 1}:0] __retval,")

      println("  input __start,")
      println("  output __idle);")
      println("  output __valid);")

      externTargets.foreach(_.emitInstance(false))

      println(s"  ${f.id} ${f.id}_inst(")
      println("    .__clk(__clk),")
      println("    .__resetn(__resetn),")

      externTargets.foreach { f =>
        f.parameters.foreach { p =>
          println(s"    .__p_${p.id}_${f.id}(__p_${p.id}_${f.id}),")
        }

        if (f.returnSize > 0)
          println(s"    .__retval_${f.id}(__retval_${f.id}),")

        println(s"    .__start_${f.id}(__start_${f.id}),")
        println(s"    .__idle_${f.id}(__idle_${f.id}),")
        println(s"    .__valid_${f.id}(__valid_${f.id}),")
      }

      f.parameters.foreach { p =>
        println(s"    .__p_${p.id}(__p_${p.id}),")
      }

      if (f.returnSize > 0)
        println(s"    .__reval(__reval),")

      println("    .__start(__start),")
      println("    .__idle(__idle),")
      println("    .__valid(__valid));")

      println("")
      println("endmodule")
    }
  }
}

case class Function(id: String,
                    returnSize: Int,
                    parameters: IndexedSeq[Parameter],
                    var body: Option[FunctionBody]) {
  def check(): Unit = {
    body.foreach(_.check(this))
  }

  def pretty(): Unit = {
    print(s"i$returnSize $id(${
      parameters.map(p => s"i${p.size} ${p.id}")
        .mkString(", ")
    })")
    body match {
      case Some(b) =>
        println(" {")
        b.stmts.foreach(_.pretty())
        println("}")

      case None =>
        println(";")
    }

  }

  def emit(): Unit = body.foreach(_.emit(this))

  def emitInstance(reg: Boolean): Unit = {
    println("")

    val t = if (reg) "reg" else "wire"

    parameters.foreach { p =>
      println(s"  $t [${p.size - 1}:0] __p_${p.id}_$id;")
    }

    if (returnSize > 0)
      println(s"  wire [${returnSize - 1}:0] __retval_$id;")

    println(s"  $t __start_$id;")
    println(s"  wire __idle_$id;")
    println(s"  wire __valid_$id;")

    println(s"  $id __inst_$id(")
    println("    .__clk(__clk),")
    println("    .__resetn(__resetn),")

    parameters.foreach { p =>
      println(s"    .__p_${p.id}(__p_${p.id}_$id),")
    }

    if (returnSize > 0)
      println(s"    .__retval(__retval_$id),")

    println(s"    .__start(__start_$id),")
    println(s"    .__idle(__idle_$id),")
    println(s"    .__valid(__valid_$id));")

    println("")
  }
}

object CompilationUnit {
  def example(): CompilationUnit = {
    val writeUart = Function("write_uart",
      0,
      IndexedSeq(new Parameter("c", 8)),
      None)

    val c = new Local("c", 8)

    val M = new Label("M")
    val N = new Label("N")
    val O = new Label("O")

    val stmts = IndexedSeq(
      new Assign(c, Literal(8, 0x61)),
      new Goto(M),

      new Target(M),
      new Branch(RelationalExpr(Ref(c), Literal(8, 0x7a), "<=", isSigned = true), N, O),

      new Target(N),
      new Call(writeUart, None, IndexedSeq(
        Ref(c))),
      new Assign(c, AdditiveExpr(Ref(c), Literal(8, 1), "+")),
      new Goto(M),

      new Target(O),
      new Call(writeUart, None, IndexedSeq(
        Literal(8, 0xd))),
      new Call(writeUart, None, IndexedSeq(
        Literal(8, 0xa))),
      new Return(None))

    val writeAlphabet = Function("write_alphabet",
      0,
      IndexedSeq(),
      Some(FunctionBody(IndexedSeq(c), stmts)))

    val cu = CompilationUnit(Seq(writeUart, writeAlphabet))
    cu.check()
    cu
  }
}

case class CompilationUnit(functions: Seq[Function]) {
  def check(): Unit = functions.foreach(_.check())

  def emit(): Unit = functions.foreach(_.emit())

  def pretty(): Unit = functions.foreach(_.pretty())
}
