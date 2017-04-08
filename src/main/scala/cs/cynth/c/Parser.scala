package cs.cynth.c

import java.io.FileReader

import cs.cynth.rtl

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

class ParseError(msg: String) extends Exception(msg)

class RichPosition(pos: Position) {
  def fmt(msg: String, level: String = "error"): String = {
    val lineContents = pos.longString.split("\n").head

    s"""${Parser.file}:${pos.line}: $level: $msg
$lineContents
${lineContents.take(pos.column - 1).map { c => if (c == '\t') c else ' ' }}^
"""
  }

  def parseError(msg: String): Nothing = Parser.parseError(fmt(msg))
}

object Type {
  val bool: Type = TUInt(1)
  val char: Type = TInt(8)
  val unsignedChar: Type = TUInt(8)
  val signedChar: Type = TInt(8)
  val short: Type = TInt(16)
  val unsignedShort: Type = TInt(16)
  val int: Type = TInt(32)
  val unsigned: Type = TUInt(32)
  val long: Type = TInt(64)
  val unsignedLong: Type = TUInt(64)
}

sealed abstract class Type {
  def size: Int

  def integerPromotion(): Type = {
    if (size < Type.int.size)
      Type.int
    else
      this
  }

  def arithmeticConversion(other: Type): Type = {
    if (size > other.size)
      this
    else if (other.size > size)
      other
    else if (isSigned)
      other
    else
      this
  }

  def isSigned: Boolean = (this: @unchecked) match {
    case _: TInt => true
    case _: TUInt => false
  }
}

case class TInt(size: Int) extends Type

case class TUInt(size: Int) extends Type

case object TVoid extends Type {
  def size: Int = 0
}

case class TFunction(returnTyp: Type,
                     parameters: IndexedSeq[Parameter]) extends Type {
  def size: Int = ???
}

object Expr {
  def literal(typ: Type, i: Integer): Expr =
    VExpr(typ, rtl.Literal(typ.size, i))
}

object Block {
  def empty: Block = new Block(IndexedSeq())
}

class Block(val stmts: IndexedSeq[rtl.Statement]) {
  def this(s: rtl.Statement) = this(IndexedSeq(s))

  def ++(b: Block) = new Block(stmts ++ b.stmts)

  def ++(e: Expr): Expr = e.prepend(this)

  def ++(s: rtl.Statement) = new Block(stmts :+ s)
}

abstract class Expr {
  // value interface
  def typ: Type

  def valueExpr: VExpr

  def boolExpr: BExpr

  def convert(newTyp: Type): VExpr = {
    val ve = valueExpr
    VExpr(newTyp,
      ve.b,
      if (newTyp.size == typ.size)
        ve.expr
      else if (newTyp.size < typ.size)
        rtl.Truncate(newTyp.size, ve.expr)
      else
        (typ: @unchecked) match {
          case TInt(_) =>
            rtl.SignExtend(newTyp.size, ve.expr)
          case TUInt(_) =>
            rtl.ZeroExtend(newTyp.size, ve.expr)
          // FIXME check
        })
  }

  def integerPromotion(): VExpr = convert(typ.integerPromotion()).valueExpr

  def arithmeticConversion(other: Expr): (VExpr, VExpr) = {
    val p = integerPromotion()
    val pother = other.integerPromotion()

    val newTyp = p.typ.arithmeticConversion(pother.typ)
    (p.convert(newTyp), pother.convert(newTyp))
  }

  def binaryArithOp(rhs: Expr)(rtlOp: (rtl.Expr, rtl.Expr) => rtl.Expr): Expr = {
    val (cl, cr) = arithmeticConversion(rhs)
    VExpr(cl.typ,
      cl.b ++ cr.b,
      rtlOp(cl.expr, cr.expr))
  }

  def prepend(before: Block): Expr

  def effect(): Block

  def ++(after: Block): Block = effect() ++ after
}

object VExpr {
  def apply(typ: Type, expr: rtl.Expr): VExpr = VExpr(typ, Block.empty, expr)
}

case class VExpr(typ: Type,
                 b: Block,
                 expr: rtl.Expr) extends Expr {
  def valueExpr: VExpr = this

  def boolExpr: BExpr = {
    val Ltrue = new rtl.Label(rtl.genLabel())
    val Lfalse = new rtl.Label(rtl.genLabel())

    BExpr(typ,
      b ++
        new rtl.Branch(
          rtl.RelationalExpr(expr, rtl.Literal(typ.size, 0), "!=", isSigned = false),
          Ltrue,
          Lfalse),
      new rtl.Target(Ltrue),
      new rtl.Target(Lfalse))
  }

  def prepend(before: Block): Expr = copy(b = before ++ b)

  def effect(): Block = b
}

case class BExpr(typ: Type,
                 b: Block,
                 trueTarget: rtl.Target,
                 falseTarget: rtl.Target) extends Expr {
  def valueExpr: VExpr = {
    val t = Local.gen(typ, "t")
    val Lafter = new rtl.Label(rtl.genLabel())
    (b ++
      trueTarget ++
      t.assign(Expr.literal(typ, 1)) ++
      new rtl.Goto(Lafter) ++
      falseTarget ++
      t.assign(Expr.literal(typ, 0)) ++
      new rtl.Target(Lafter) ++
      t.ref()).valueExpr
  }

  def boolExpr: BExpr = this

  def prepend(before: Block): Expr = copy(b = before ++ b)

  def effect(): Block = {
    b ++
      trueTarget ++
      // fall through
      falseTarget
  }
}


sealed abstract class Decl {
  def pos: Position

  def id: String

  def typ: Type

}

// FIXME Variable shouldn't extend Decl, mix into Local, Parameter
abstract class Variable extends Decl {
  val v: rtl.Variable

  def assign(e: Expr): Block = {
    val c = e.convert(typ)
    c.b ++ new rtl.Assign(v, c.expr)
  }

  def ref(): Expr = VExpr(typ, rtl.Ref(v))
}

object Local {
  def gen(vtyp: Type, root: String): Variable = {
    val vid = rtl.gensym(root)
    new Variable {
      def pos: Position = ???

      def id: String = vid

      def typ: Type = vtyp

      val v = new rtl.Local(id, vtyp.size)
    }
  }
}

case class Local(pos: Position, id: String, typ: Type) extends Variable {
  val v = new rtl.Local(id, typ.size)
}

case class Parameter(pos: Position, id: String, typ: Type) extends Variable {
  val v = new rtl.Parameter(id, typ.size)
}

case class Function(pos: Position, id: String, typ: TFunction) extends Decl {

  val rtlFunction: rtl.Function =
    rtl.Function(id,
      typ.returnTyp.size,
      typ.parameters.map(_.v),
      None)

  def functionScope(parent: FileScope): FunctionScope =
    new FunctionScope(parent, this)
}

class Label(id: String) {
  val rtlLabel: rtl.Label = new rtl.Label(id)
}

abstract class Scope {
  val decls: mutable.Map[String, Decl] = mutable.Map.empty

  def parent: Scope

  def lookup(pos: Position, id: String): Decl = {
    decls.get(id) match {
      case Some(decl) => decl
      case None =>
        if (parent != null)
          parent.lookup(pos, id)
        else
          pos.parseError(s"use of undeclared identifier '$id'")
    }
  }
}

class FileScope extends Scope {
  def parent: Scope = null

  def declareFunction(pos: Position, id: String, typ: TFunction): Function = {
    val f = Function(pos, id, typ)
    decls += id -> f
    f
  }
}

abstract class FScope extends Scope {
  def functionScope: FunctionScope

  def declareLocal(pos: Position, id: String, typ: Type): Local = {
    decls.get(id) match {
      case Some(decl) =>
        Parser.parseError(
          pos.fmt(s"redefinition of '$id'") +
            decl.pos.fmt("previous definition is here", level = "note"))

      case None =>
        val l = Local(pos, id, typ)
        decls += id -> l
        l
    }
  }
}

class FunctionScope(val parent: FileScope,
                    val function: Function) extends FScope {
  // declare parameters
  function.typ.parameters.foreach { p =>
    decls += p.id -> p
  }

  val labels: mutable.Map[String, Label] = mutable.Map.empty

  def functionScope: FunctionScope = this

  def getLabel(id: String): Label = {
    labels.get(id) match {
      case Some(label) => label
      case None =>
        val label = new Label(id)
        labels += id -> label
        label
    }
  }
}

class BlockScope(val parent: FScope) extends FScope {
  def functionScope: FunctionScope = parent.functionScope
}

case class Positioned[T](value: T) extends Positional {
  def error(msg: String): Nothing = pos.parseError(msg)
}

object Parser extends RegexParsers {
  var file: String = "<input>"

  def parseError(msg: String): Nothing = throw new ParseError(msg)

  def parseString(input: String): rtl.CompilationUnit = {
    file = "<input>"

    parseAll(compilation_unit, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) =>
        throw new ParseError(msg)
    }
  }

  def parseFile(f: String): rtl.CompilationUnit = {
    file = f

    parseAll(compilation_unit, new FileReader(f)) match {
      case Success(result, _) => result
      case NoSuccess(msg, next) =>
        next.pos.parseError(msg)
    }
  }

  def pos[T](p: => Parser[T]): Parser[Positioned[T]] =
    positioned[Positioned[T]](p ^^ { x => Positioned(x) })

  // C99 reserved keywords
  val reservedKeywords = Set("auto", "enum", "break", "extern", "case", "float", "char", "for", "const", "goto",
    "continue", "if", "default", "inline", "do", "int", "double", "long", "else", "register", "restrict", "return",
    "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile",
    "while", "_Bool", "_Complex", "_Imaginary")

  // FIXME standard C types
  def typ: Parser[Type] =
    "int[0-9]+".r ^^ { s => TInt(s.drop(3).toInt) } |
      "uint[0-9]+".r ^^ { s => TUInt(s.drop(4).toInt) } |
      "char" ^^ { _ => Type.char } |
      "short" ^^ { _ => Type.short } |
      "int" ^^ { _ => Type.int } |
      "unsigned" ^^ { _ => Type.unsigned } |
      "long" ^^ { _ => Type.long } |
      // FIXME _Bool type?
      "_Bool" ^^ { _ => Type.bool } |
      "void" ^^ { _ => TVoid }

  def ident: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.filter(id =>
      !reservedKeywords.contains(id) &&
        // FIXME
        !id.matches("u?int[0-9]+")
    )

  def expr(scope: FScope): Parser[Expr] = assign_expr(scope)

  def assign_expr(scope: FScope): Parser[Expr] =
    (pos(ident) <~ "=") ~ assign_expr(scope) ^^ { case id ~ e =>
      val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]
      val c = e.convert(v.typ)
      v.assign(c) ++ v.ref()
    } |
      cond_expr(scope)

  def cond_expr(scope: FScope): Parser[Expr] =
    ior_expr(scope) ~ pos("?") ~ (expr(scope) <~ ":") ~ cond_expr(scope) ^^ { case c ~ q ~ t ~ f =>
      val Lafter = new rtl.Label(rtl.genLabel())

      val bc = c.boolExpr
      val (pt, pf) = t.arithmeticConversion(f)
      val r = Local.gen(pt.typ, "mux")

      bc.b ++
        bc.trueTarget ++
        r.assign(pt) ++
        new rtl.Goto(Lafter) ++
        bc.falseTarget ++
        r.assign(pf) ++
        // fall through
        new rtl.Target(Lafter) ++
        r.ref()
    } |
      logical_or_expr(scope)

  def logical_or_expr(scope: FScope): Parser[Expr] =
    logical_and_expr(scope) ~ rep("||" ~> logical_and_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l.boolExpr) { case (l, r) =>
        val br = r.boolExpr
        BExpr(Type.int,
          l.b ++
            l.falseTarget ++
            br.b ++
            br.trueTarget ++
            new rtl.Goto(l.trueTarget.label),
          l.trueTarget,
          br.falseTarget)
      }
    }

  def logical_and_expr(scope: FScope): Parser[Expr] =
    ior_expr(scope) ~ rep("&&" ~> ior_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l.boolExpr) { case (l, r) =>
        val br = r.boolExpr
        BExpr(Type.int,
          l.b ++
            l.trueTarget ++
            br.b ++
            br.falseTarget ++
            new rtl.Goto(l.falseTarget.label),
          br.trueTarget,
          l.falseTarget)
      }
    }

  def ior_expr(scope: FScope): Parser[Expr] =
    xor_expr(scope) ~ rep("|" ~ xor_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        VExpr(pl.typ,
          pl.b ++ pr.b,
          rtl.BinaryLogicalExpr(pl.expr, pr.expr, "|"))
      }
    }

  def xor_expr(scope: FScope): Parser[Expr] =
    and_expr(scope) ~ rep("^" ~ and_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        VExpr(pl.typ,
          pl.b ++ pr.b,
          rtl.BinaryLogicalExpr(pl.expr, pr.expr, "^"))
      }
    }

  def and_expr(scope: FScope): Parser[Expr] =
    equality_expr(scope) ~ rep("&" ~ equality_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        VExpr(pl.typ,
          pl.b ++ pr.b,
          rtl.BinaryLogicalExpr(pl.expr, pr.expr, "&"))
      }
    }

  def equality_expr(scope: FScope): Parser[Expr] =
    relational_expr(scope) ~ rep(("==" | "!=") ~ relational_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        VExpr(TInt(32),
          pl.b ++ pr.b,
          rtl.ZeroExtend(32,
            rtl.RelationalExpr(pl.expr, pr.expr, op, pl.typ.isSigned)))
      }
    }

  def relational_expr(scope: FScope): Parser[Expr] =
    shift_expr(scope) ~ rep(("<=" | "<" | ">=" | ">") ~ shift_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        VExpr(TInt(32),
          pl.b ++ pr.b,
          rtl.ZeroExtend(32,
            rtl.RelationalExpr(pl.expr, pr.expr, op, pl.typ.isSigned)))
      }
    }

  def shift_expr(scope: FScope): Parser[Expr] =
    additive_expr(scope) ~ rep(("<<" | ">>") ~ int_literal) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (_, dist) = r
        val lp = l.integerPromotion()
        VExpr(lp.typ,
          lp.b,
          op match {
            case "<<" => rtl.ShiftLeft(lp.expr, dist)
            case ">>" =>
              if (lp.typ.isSigned)
                rtl.ShiftRightArithmetic(lp.expr, dist)
              else
                rtl.ShiftRightLogical(lp.expr, dist)
          })
      }
    }

  def additive_expr(scope: FScope): Parser[Expr] =
    mult_expr(scope) ~ rep(("+" | "-") ~ mult_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        VExpr(pl.typ,
          pl.b ++ pr.b,
          rtl.AdditiveExpr(pl.expr, pr.expr, op))
      }
    }

  def mult_expr(scope: FScope): Parser[Expr] =
    unary_expr(scope)

  def cast_expr(scope: FScope): Parser[Expr] =
    unary_expr(scope) |
      ("(" ~> typ) ~ (")" ~> unary_expr(scope)) ^^ { case typ ~ e =>
        e.convert(typ)
      }

  def unary_expr(scope: FScope): Parser[Expr] =
    ("++" | "--") ~ pos(ident) ^^ { case op ~ id =>
      val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]
      v.assign(
        v.ref().binaryArithOp(Expr.literal(Type.int, 1))((l, r) =>
          rtl.AdditiveExpr(l, r, op))) ++
        v.ref()
    } |
      ("-" | "+" | "~" | "!") ~ cast_expr(scope) ^^ { case op ~ e =>
        val p = e.integerPromotion()
        op match {
          case "-" => VExpr(p.typ, p.b, rtl.Neg(p.expr))
          case "+" => p
          case "~" => VExpr(p.typ, p.b, rtl.Not(p.expr))
          case "!" =>
            val bp = p.boolExpr
            BExpr(bp.typ,
              bp.b,
              bp.falseTarget,
              bp.trueTarget)
        }
      } |
      "sizeof" ~> expr(scope) ^^ { e =>
        Expr.literal(Type.int, e.typ.size)
      } |
      (("sizeof" ~ "(") ~> typ) <~ ")" ^^ { typ =>
        Expr.literal(Type.int, typ.size)
      } |
      postfix_expr(scope)

  def postfix_expr(scope: FScope): Parser[Expr] =
    (pos(ident) <~ "(") ~ repsep(expr(scope), ",") <~ ")" ^^ { case id ~ args =>
      // FIXME check
      val f = scope.lookup(id.pos, id.value).asInstanceOf[Function]
      // FIXME void
      val retval = Local.gen(f.typ.returnTyp, "retval")
      val vargs = args.map(_.valueExpr)
      vargs.foldLeft(Block.empty)(_ ++ _.b) ++
        new rtl.Call(f.rtlFunction, Some(retval.v),
          (f.typ.parameters, vargs).zipped.map {
            case (p, a) =>
              a.convert(p.typ).expr
          }.toIndexedSeq) ++
        retval.ref()
    } | pos(ident) ~ ("++" | "--") ^^ { case id ~ op =>
      val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]
      val t = Local.gen(v.typ, "postfix")
      t.assign(v.ref()) ++
        v.assign(
          v.ref().binaryArithOp(Expr.literal(Type.int, 1))((l, r) =>
            rtl.AdditiveExpr(l, r, if (op == "++") "+" else "-"))) ++
        t.ref()
    } |
      primary_expr(scope)

  def primary_expr(scope: FScope): Parser[Expr] =
    "(" ~> expr(scope) <~ ")" |
      pos(ident) ^^ { id =>
        // FIXME check
        val decl = scope.lookup(id.pos, id.value)
        val v = decl.asInstanceOf[Variable]
        v.ref()
      } |
      int_literal ^^ { case (size, i) =>
        Expr.literal(TInt(size), i)
      }

  def int_literal: Parser[(Int, Int)] =
    "0x[0-9a-fA-F]+".r ^^ { s =>
      (32, java.lang.Integer.parseInt(s.drop(2), 16))
    } |
      "[0-9]+".r ^^ { s => (32, s.toInt) } |
      "'[^'\\\\\n]'".r ^^ { s => (8, s.apply(1).toInt) } |
      "'\\\\['\\\\\"?abfnrtv]'".r ^^ { s =>
        val c = s.apply(2) match {
          case '\'' => '\''
          case '\\' => '\\'
          case '"' => '"'
          case '?' => '?'
          case 'a' => '\u0007'
          case 'b' => '\b'
          case 'f' => '\f'
          case 'n' => '\n'
          case 'r' => '\r'
          case 't' => '\t'
          case 'v' => '\u000b'
        }

        (8, c.toInt)
      }

  def stmt(scope: FScope): Parser[Block] =
    ("if" ~> "(" ~> expr(scope) <~ ")") ~ stmt(scope) ~ opt("else" ~> stmt(scope)) ^^ {
      case cond ~ thenStmt ~ elseStmt =>
        val bcond = cond.boolExpr
        elseStmt match {
          case Some(elseStmt) =>
            val Lafter = new rtl.Label(rtl.genLabel())

            bcond.b ++
              bcond.trueTarget ++
              thenStmt ++
              new rtl.Goto(Lafter) ++
              bcond.falseTarget ++
              elseStmt ++
              bcond.falseTarget

          case None =>
            bcond.b ++
              bcond.trueTarget ++
              thenStmt ++
              bcond.falseTarget
        }
    } |
      ident <~ ":" ^^ { id =>
        // FIXME check
        val label = scope.functionScope.getLabel(id)
        new Block(new rtl.Target(label.rtlLabel))
      } |
      expr(scope) <~ ";" ^^ {
        expr => expr.effect()
      } |
      "goto" ~> ident <~ ";" ^^ { id =>
        // FIXME check
        val label = scope.functionScope.getLabel(id)
        new Block(new rtl.Goto(label.rtlLabel))
      } |
      // FIXME intializer
      typ ~ pos(ident) <~ ";" ^^ { case typ ~ id =>
        if (typ == TVoid)
          id.pos.parseError(s"variable '${
            id.value
          }' may not have 'void' type")

        scope.declareLocal(id.pos, id.value, typ)
        Block.empty
      } |
      block_stmt(scope) |
      "return" ~> opt(expr(scope)) <~ ";" ^^ {
        case Some(e) =>
          val c = e.convert(scope.functionScope.function.typ.returnTyp).valueExpr
          c.b ++
            new rtl.Return(Some(c.expr))
        case None =>
          new Block(new rtl.Return(None))
      }

  def block_stmt(scope: FScope): Parser[Block] = {
    val bscope = new BlockScope(scope)
    "{" ~> rep(stmt(bscope)) <~ "}" ^^ {
      _.foldLeft(Block.empty)(_ ++ _)
    }
  }

  def param: Parser[Parameter] =
    typ ~ pos(ident) ^^ { case t ~ id =>
      if (t == TVoid)
        id.pos.parseError(s"parameter '${
          id.value
        }' may not have 'void' type")

      Parameter(id.pos, id.value, t)
    }

  def parameters: Parser[IndexedSeq[Parameter]] =
    repsep(param, ",") ^^ {
      _.toIndexedSeq
    }

  def function_head(fileScope: FileScope): Parser[Function] =
    typ ~ pos(ident) ~ ("(" ~> parameters <~ ")") ^^ { case returnTyp ~ id ~ params =>
      fileScope.declareFunction(id.pos, id.value, TFunction(returnTyp, params))
    }

  def function_body(fileScope: FileScope, f: Function): Parser[Block] = {
    val fscope = f.functionScope(fileScope)
    "{" ~> rep(stmt(fscope)) <~ "}" ^^ {
      _.foldLeft(Block.empty)(_ ++ _)
    }
  }

  def function(fileScope: FileScope): Parser[Function] =
    function_head(fileScope)
      .flatMap { f =>
        (";" |
          function_body(fileScope, f) ^^ {
            b =>
              f.rtlFunction.body = Some(rtl.FunctionBody(
                IndexedSeq(),
                b.stmts))
          }) ^^ { _ => f }
      }

  def compilation_unit: Parser[rtl.CompilationUnit] = {
    val fileScope = new FileScope
    rep(function(fileScope)) ^^ { _ =>
      new rtl.CompilationUnit(fileScope.decls.values
        .map(_.asInstanceOf[Function].rtlFunction)
        .toSeq)
    }
  }
}
