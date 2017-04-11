package cs.cynth.rtl

import java.io.PrintStream

import scala.collection.mutable

class EmitContext {
  val decls: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
  val stmts: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty

  def emit(out: PrintStream): Unit = {
    decls.foreach(out.println)
    stmts.foreach(out.println)
  }
}


sealed abstract class Expr {
  def size: Int

  def children: IndexedSeq[Expr]

  def check(): Unit = {
    children.foreach(_.check())
  }

  def pretty(): Unit = print(toString)

  def variables(): Set[Variable] =
    children.flatMap(_.variables()).toSet

  def emit(c: EmitContext): String
}

case class Literal(size: Int, value: Int) extends Expr {
  def children = IndexedSeq()

  def emit(c: EmitContext) = s"$size'd$value"
}

case class Ref(v: Variable) extends Expr {
  def children = IndexedSeq()

  def size: Int = v.size

  def emit(c: EmitContext) = s"${v.id}"

  override def variables(): Set[Variable] = Set(v)
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
  def emit(c: EmitContext) = s"(${left.emit(c)}) $op (${right.emit(c)})"
}

case class Neg(child: Expr) extends UnaryArithExpr {
  def emit(c: EmitContext) = s"- (${child.emit(c)})"
}

case class Not(child: Expr) extends UnaryArithExpr {
  override def emit(c: EmitContext) = s"~ (${}child.emit(c)})"
}

case class RelationalExpr(left: Expr, right: Expr, op: String, isSigned: Boolean) extends CompareExpr {
  override def emit(c: EmitContext): String = {
    val s = if (isSigned) "$signed" else ""
    s"$s(${left.emit(c)}) $op $s(${right.emit(c)})"
  }
}

case class BinaryLogicalExpr(left: Expr, right: Expr, op: String) extends BinaryExpr {
  def size: Int = left.size

  def emit(c: EmitContext) =
    s"(${left.emit(c)}) $op (${right.emit(c)})"
}

case class ShiftLeft(child: Expr, right: Int) extends UnaryExpr {
  def size: Int = child.size

  def emit(c: EmitContext): String = {
    val t = gensym("t")
    c.decls += s"          reg [${child.size - 1}:0] $t;"
    c.stmts += s"          $t = ${child.emit(c)};"

    if (right >= size)
      s"$size'd0"
    else
      s"{$t[${size - 1}:$right], $right'd0}"
  }
}

case class ShiftRightLogical(child: Expr, right: Int) extends UnaryExpr {
  def size: Int = child.size

  def emit(c: EmitContext): String = {
    val t = gensym("t")
    c.decls += s"          reg [${child.size - 1}:0] $t;"
    c.stmts += s"          $t = ${child.emit(c)};"

    if (right >= size)
      s"$size'd0"
    else
      s"{$right'd0, $t[${size - right - 1}:0]}"
  }
}

case class ShiftRightArithmetic(child: Expr, right: Int) extends UnaryExpr {
  def size: Int = child.size

  override def emit(c: EmitContext): String = {
    val t = gensym("t")
    c.decls += s"          reg [${child.size - 1}:0] $t;"
    c.stmts += s"          $t = ${child.emit(c)};"

    if (right >= size)
      s"$size'd0"
    else
      s"{{$right{$t[${size - 1}]}}, $t[${size - right - 1}:0]}"
  }
}

case class Mux(cond: Expr, left: Expr, right: Expr) extends Expr {
  def children = IndexedSeq(cond, left, right)

  override def check(): Unit = {
    assert(left.size == right.size)
    assert(cond.size == 1)
  }

  def size: Int = left.size

  def emit(c: EmitContext): String = s"(${cond.emit(c)}) ? (${left.emit(c)}) : (${right.emit(c)})"
}

case class SignExtend(size: Int, child: Expr) extends UnaryExpr {
  override def check(): Unit = {
    super.check()
    assert(child.size < size)
  }

  def emit(c: EmitContext): String = {
    val t = gensym("t")
    c.decls += s"          reg [${child.size - 1}:0] $t;"
    c.stmts += s"          $t = ${child.emit(c)};"

    s"{{${size - child.size}{$t[${child.size - 1}]}}, $t}"
  }
}

case class ZeroExtend(size: Int, child: Expr) extends UnaryExpr {
  override def check(): Unit = {
    super.check()
    assert(child.size < size)
  }

  def emit(c: EmitContext) = s"{${size - child.size}'d0, (${child.emit(c)})}"
}

case class Truncate(size: Int, child: Expr) extends UnaryExpr {
  override def check(): Unit = {
    super.check()
    assert(size < child.size)
  }

  def emit(c: EmitContext) = {
    val t = gensym("t")
    c.decls += s"          reg [${child.size - 1}:0] $t;"
    c.stmts += s"          $t = ${child.emit(c)};"

    s"$t[${size - 1}:0]"
  }
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

  def emit(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    val blockLabel = gensym("block")

    out.println(s"        $id : begin : $blockLabel")
    emitBody(out: PrintStream, b, next)
    out.println("        end")
  }

  def emitBody(out: PrintStream, b: FunctionBody, next: Int): Unit =
    throw new UnsupportedOperationException

  def variables(): Set[Variable] = Set.empty
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

  override def emitBody(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    val c = new EmitContext()
    val exprS = expr.emit(c)
    c.emit(out)
    out.println(s"          ${v.id} <= $exprS;")
    out.println(s"          __state <= $next;")
  }

  override def variables(): Set[Variable] = expr.variables() + v
}

class Label(val id: String, var target: Target) {
  def this(id: String) = this(id, null)
}

class Goto(label: Label) extends Statement {
  override def emitBody(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    out.println(s"          __state <= ${label.target.id};")
  }

  def pretty(): Unit = {
    println(s"  goto ${label.id};")
  }
}

class Target(val label: Label) extends Statement {
  assert(label.target == null)
  label.target = this

  override def check(f: Function): Unit = {
    assert(label.target != null)
  }

  def pretty(): Unit = {
    println(s" ${label.id}:")
  }

  override def emitBody(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    out.println(s"          __state <= $next;")
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

  override def emitBody(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    val c = new EmitContext()
    val condS = cond.emit(c)
    c.emit(out)
    out.println(s"          if ($condS)")
    out.println(s"            __state <= ${thenLabel.target.id};")
    out.println("          else")
    out.println(s"            __state <= ${elseLabel.target.id};")
  }

  override def variables(): Set[Variable] = cond.variables()
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

  override def emit(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    val validState = Statement.newId()

    out.println(s"        $id : begin")

    out.println(s"          __start_${target.id} <= 1;")

    val c = new EmitContext()
    val argumentsS = arguments.map(_.emit(c))
    c.emit(out)
    (target.parameters, argumentsS).zipped.foreach { (p, a) =>
      out.println(s"          __p_${p.id}_${target.id} <= $a;")
    }

    out.println(s"          __state <= $validState;")
    out.println("        end")

    out.println(s"        $validState : begin")
    out.println(s"          __start_${target.id} <= 0;")
    out.println(s"          if (__valid_${target.id}) begin")
    returnVariable.foreach { v =>
      out.println(s"            ${v.id} <= __retval_${target.id};")
    }
    out.println(s"            __state <= $next;")
    out.println(s"          end")
    out.println("        end")
  }

  override def variables(): Set[Variable] =
    arguments.flatMap(_.variables()).toSet ++ returnVariable
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

  override def emitBody(out: PrintStream, b: FunctionBody, next: Int): Unit = {
    expr.foreach { e =>
      val c = new EmitContext()
      val eS = e.emit(c)
      c.emit(out)
      out.println(s"          __retval <= $eS;")
    }

    out.println(s"          __state <= ${b.returnState};")
  }

  override def variables(): Set[Variable] =
    expr.map(_.variables()).getOrElse(Set.empty)
}

case class FunctionBody(stmts: IndexedSeq[Statement]) {
  val idleState: Int = Statement.newId()
  val returnState: Int = Statement.newId()

  def initialState: Int =
    stmts.headOption.map(_.id).getOrElse(returnState)

  def check(f: Function): Unit = {
    stmts.foreach(_.check(f))
  }

  def emit(out: PrintStream, f: Function): Unit = {

    val targets: Set[Function] =
      stmts.flatMap {
        case c: Call =>
          Some(c.target)
        case _ => None
      }.toSet

    val nonstdTargets = targets.filter(f => f.nonstd)

    out.println(s"module ${f.id}(")
    out.println("  input __clk,")
    out.println("  input __resetn,")

    nonstdTargets
      .foreach { f =>
        out.println("")
        out.println(s"  // ${f.id} interface")

        f.parameters.foreach { p =>
          out.println(s"  output reg [${p.size - 1}:0] __p_${p.id}_${f.id},")
        }

        if (f.returnSize > 0)
          out.println(s"  wire [${f.returnSize - 1}:0] __retval_${f.id},")

        out.println(s"  output reg __start_${f.id},")
        out.println(s"  input __idle_${f.id},")
        out.println(s"  input __valid_${f.id},")

        out.println("")
      }

    f.parameters.foreach { p =>
      out.println(s"  input [${p.size - 1}:0] __p_${p.id},")
    }

    if (f.returnSize > 0)
      out.println(s"  output reg [${f.returnSize - 1}:0] __retval,")

    out.println("  input __start,")
    out.println("  output __idle,")
    out.println("  output __valid);")

    out.println("  reg [31:0] __state;")

    val vars = stmts.flatMap(_.variables()).toSet
    vars.foreach { v =>
      out.println(s"  reg [${v.size - 1}:0] ${v.id};")
    }

    out.println(s"  assign __idle = (__state == $idleState);")
    out.println(s"  assign __valid = (__state == $returnState);")

    targets
      .filter(f => !f.nonstd)
      .foreach(_.emitInstance(out, reg = true))

    out.println("  always @(posedge __clk) begin")
    out.println("    if (!__resetn) begin")
    out.println(s"      __state <= $idleState;")

    targets.foreach { f =>
      out.println(s"      __start_${f.id} <= 0;")
    }

    out.println("    end else begin")
    out.println("      case (__state)")
    out.println(s"        $idleState : begin")
    out.println("          if (__start)")
    out.println(s"            __state <= $initialState;")
    f.parameters.foreach { p =>
      out.println(s"            ${p.id} <= __p_${p.id};")
    }
    out.println("        end")

    stmts.zipWithIndex.foreach { case (s, i) =>
      val next =
        if (i + 1 < stmts.length)
          stmts(i + 1).id
        else
          returnState

      s.emit(out, this, next)
    }

    out.println(s"        $returnState : begin")
    out.println(s"          __state <= $idleState;")
    out.println("        end")

    out.println("      endcase")
    out.println("    end")
    out.println("  end")

    out.println("")

    out.println("endmodule")
  }
}

case class Function(id: String,
                    returnSize: Int,
                    parameters: IndexedSeq[Parameter],
                    nonstd: Boolean,
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

  def emit(out: PrintStream): Unit = body.foreach(_.emit(out, this))

  def emitInstance(out: PrintStream, reg: Boolean): Unit = {
    out.println("")

    val t = if (reg) "reg" else "wire"

    parameters.foreach { p =>
      out.println(s"  $t [${p.size - 1}:0] __p_${p.id}_$id;")
    }

    if (returnSize > 0)
      out.println(s"  wire [${returnSize - 1}:0] __retval_$id;")

    out.println(s"  $t __start_$id;")
    out.println(s"  wire __idle_$id;")
    out.println(s"  wire __valid_$id;")

    out.println(s"  $id __inst_$id(")
    out.println("    .__clk(__clk),")
    out.println("    .__resetn(__resetn),")

    parameters.foreach { p =>
      out.println(s"    .__p_${p.id}(__p_${p.id}_$id),")
    }

    if (returnSize > 0)
      out.println(s"    .__retval(__retval_$id),")

    out.println(s"    .__start(__start_$id),")
    out.println(s"    .__idle(__idle_$id),")
    out.println(s"    .__valid(__valid_$id));")

    out.println("")
  }
}

case class CompilationUnit(functions: Seq[Function]) {
  def check(): Unit = functions.foreach(_.check())

  def emit(out: PrintStream): Unit = functions.foreach(_.emit(out))

  def pretty(): Unit = functions.foreach(_.pretty())
}
