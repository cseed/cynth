package cs.cynth.c

import java.io.FileReader

import cs.cynth.rtl

import scala.collection.mutable
import scala.util.matching.Regex
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

  def error(msg: String): Nothing = Parser.error(fmt(msg))

  def warning(msg: String): Unit = {
    System.err.println(fmt(msg, level = "warning"))
  }
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
  val longLong: Type = TInt(64)
  val unsignedLongLong: Type = TUInt(64)
}

sealed abstract class DeclSpec {
  def isConcreteTypeSpecifier: Boolean
}

object DeclSpec {

  case object Bool extends DeclSpec {
    override def toString: String = "_Bool"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object Void extends DeclSpec {
    override def toString: String = "void"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object Char extends DeclSpec {
    override def toString: String = "char"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object Short extends DeclSpec {
    override def toString: String = "short"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object Int extends DeclSpec {
    override def toString: String = "int"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object Long extends DeclSpec {
    override def toString: String = "long"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object LongLong extends DeclSpec {
    override def toString: String = "long long"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case class SizedInt(size: Int) extends DeclSpec {
    override def toString: String = s"__int$size"

    def isConcreteTypeSpecifier: Boolean = true
  }

  case object Signed extends DeclSpec {
    override def toString: String = "signed"

    def isConcreteTypeSpecifier: Boolean = false
  }

  case object Unsigned extends DeclSpec {
    override def toString: String = "unsigned"

    def isConcreteTypeSpecifier: Boolean = false
  }

  case object Nonstd extends DeclSpec {
    override def toString: String = "__nonstd"

    def isConcreteTypeSpecifier: Boolean = false
  }

  def merge(specs: List[DeclSpec], posSpec: Positioned[DeclSpec]): List[DeclSpec] = {
    val pos = posSpec.pos
    val spec = posSpec.value

    def f(specs: List[DeclSpec]): List[DeclSpec] = specs match {
      case hd :: tl =>
        if (hd == DeclSpec.Long && spec == DeclSpec.Long)
          DeclSpec.LongLong :: tl
        else {
          if ((hd.isConcreteTypeSpecifier && spec.isConcreteTypeSpecifier) ||
            (hd == DeclSpec.Signed && spec == DeclSpec.Unsigned) ||
            (hd == DeclSpec.Unsigned && spec == DeclSpec.Signed))
            posSpec.error(s"cannot combine '$spec' with previous '$hd' declaration specifier")

          if (hd == spec) {
            posSpec.warning(s"duplicate '$spec' declaration specifier")
            tl
          } else
            hd :: f(tl)
        }

      case Nil => List(spec)
    }

    f(specs)
  }

  def isNonstd(specs: List[DeclSpec]): Boolean = specs.contains(DeclSpec.Nonstd)

  def typ(pos: Position, specs: List[DeclSpec]): Type = {
    val isUnsigned = specs.contains(DeclSpec.Unsigned)
    val isSigned = specs.contains(DeclSpec.Signed)

    def f(specs: List[DeclSpec]): Type = specs match {
      case hd :: tl => hd match {
        case DeclSpec.Void => TVoid
        case DeclSpec.Bool => Type.bool
        case DeclSpec.Char =>
          if (isUnsigned)
            Type.unsignedChar
          else if (isSigned)
            Type.signedChar
          else
            Type.char
        case DeclSpec.Short => if (isUnsigned) Type.unsignedShort else Type.short
        case DeclSpec.Int => if (isUnsigned) Type.unsigned else Type.int
        case DeclSpec.Long => if (isUnsigned) Type.unsignedLong else Type.long
        case DeclSpec.LongLong => if (isUnsigned) Type.unsignedLongLong else Type.longLong
        case DeclSpec.SizedInt(size) => if (isUnsigned) TUInt(size) else TInt(size)
        case _ => f(tl)
      }

      case Nil =>
        if (isUnsigned)
          Type.unsigned
        else {
          if (!isSigned)
            pos.warning("type specifier missing, defaulting to 'int'")
          Type.int
        }
    }

    f(specs)
  }
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

  def apply(s: rtl.Statement): Block = Block(IndexedSeq(s))
}

case class Block(stmts: IndexedSeq[rtl.Statement]) {
  def ++(b: Block) = Block(stmts ++ b.stmts)

  def ++(e: Expr): Expr = e.prepend(this)

  def ++(s: rtl.Statement) = Block(stmts :+ s)
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

trait Variable {
  def typ: Type

  val v: rtl.Variable

  def assign(e: Expr): Block = {
    val c = e.convert(typ)
    c.b ++ new rtl.Assign(v, c.expr)
  }

  def ref(): Expr = VExpr(typ, rtl.Ref(v))
}

object Local {
  def gen(vtyp: Type, root: String): Variable = {
    new Variable {
      def typ: Type = vtyp

      val v: rtl.Variable = new rtl.Local(rtl.gensym(root), vtyp.size)
    }
  }
}

case class Local(pos: Position, id: String, typ: Type) extends Decl with Variable {
  val v = new rtl.Local(id, typ.size)
}

case class Parameter(pos: Position, id: String, typ: Type) extends Decl with Variable {
  val v = new rtl.Parameter(id, typ.size)
}

case class Function(pos: Position, id: String, typ: TFunction, nonstd: Boolean) extends Decl {
  val rtlFunction: rtl.Function =
    rtl.Function(id,
      typ.returnTyp.size,
      typ.parameters.map(_.v),
      nonstd,
      None)

  def functionScope(parent: FileScope): FunctionScope =
    new FunctionScope(parent, this)
}

class Label(val id: String) {
  val rtlLabel: rtl.Label = new rtl.Label(id)

  var usePos: Position = _

  var definitionPos: Position = _
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
          pos.error(s"use of undeclared identifier '$id'")
    }
  }
}

class FileScope extends Scope {
  def parent: Scope = null

  def declareFunction(pos: Position, id: String, typ: TFunction, nonstd: Boolean): Function = {
    val f = Function(pos, id, typ, nonstd)
    decls += id -> f
    f
  }
}

abstract class FScope extends Scope {
  def functionScope: FunctionScope

  def declareLocal(pos: Position, id: String, typ: Type): Local = {
    decls.get(id) match {
      case Some(decl) =>
        Parser.error(
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

case class Positioned[+T](value: T) extends Positional {
  def error(msg: String): Nothing = pos.error(msg)

  def warning(msg: String): Unit = pos.warning(msg)
}

object Parser extends RegexParsers {
  override val whiteSpace: Regex = "\\s*//[^\n]*\\s*|\\s+".r

  var file: String = "<input>"

  def error(msg: String): Nothing = throw new ParseError(msg)

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
        next.pos.error(msg)
    }
  }

  def pos[T](p: => Parser[T]): Parser[Positioned[T]] =
    positioned[Positioned[T]](p ^^ { x => Positioned(x) })

  // C99 reserved keywords
  val reservedKeywords = Set("auto", "enum", "break", "extern", "case", "float", "char", "for", "const", "goto",
    "continue", "if", "default", "inline", "do", "int", "double", "long", "else", "register", "restrict", "return",
    "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile",
    "while", "_Bool", "_Complex", "_Imaginary")

  def at[T](pos: Position, x: T): Positioned[T] = {
    val pd = Positioned(x)
    pd.setPos(pos)
    pd
  }

  def declSpec: Parser[Positioned[DeclSpec]] =
    pos("void") ^^ { s => at(s.pos, DeclSpec.Void) } |
      pos("_Bool") ^^ { s => at(s.pos, DeclSpec.Bool) } |
      pos("char") ^^ { s => at(s.pos, DeclSpec.Char) } |
      pos("short") ^^ { s => at(s.pos, DeclSpec.Short) } |
      pos("int") ^^ { s => at(s.pos, DeclSpec.Int) } |
      pos("long") ^^ { s => at(s.pos, DeclSpec.Long) } |
      pos("signed") ^^ { s => at(s.pos, DeclSpec.Signed) } |
      pos("unsigned") ^^ { s => at(s.pos, DeclSpec.Unsigned) } |
      pos("__int[0-9]+".r) ^^ { s => at(s.pos, DeclSpec.SizedInt(s.value.drop(5).toInt)) } |
      pos("__nonstd") ^^ { s => at(s.pos, DeclSpec.Nonstd) }

  val declSpecs: Parser[List[DeclSpec]] = rep1(declSpec) ^^ { specs =>
    specs.foldLeft[List[DeclSpec]](Nil)((specs, spec) => DeclSpec.merge(specs, spec))
  }

  def ident: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.filter(id =>
      !(reservedKeywords.contains(id) ||
        id.matches("__int[0-9]+")))

  def expr(scope: FScope): Parser[Expr] = assign_expr(scope)

  def assign_expr(scope: FScope): Parser[Expr] =
    pos(ident) ~ ("=" | "+=" | "-=" | "&=" | "^=" | "|=") ~ assign_expr(scope) ^^ { case id ~ op ~ rhs =>
      val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]
      if (op == "=")
        v.assign(rhs) ++ v.ref()
      else
        v.assign(v.ref().binaryArithOp(rhs) { (l, r) =>
          op match {
            case "+=" => rtl.AdditiveExpr(l, r, "+")
            case "-=" => rtl.AdditiveExpr(l, r, "-")
            case "&=" => rtl.BinaryLogicalExpr(l, r, "&")
            case "^=" => rtl.BinaryLogicalExpr(l, r, "^")
            case "|=" => rtl.BinaryLogicalExpr(l, r, "|")
          }
        }) ++
          v.ref()
    } |
      pos(ident) ~ ("<<=" | ">>=") ~ int_literal ^^ { case id ~ op ~ rhs =>
        val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]
        val (_, dist) = rhs
        val lp = v.ref().integerPromotion()
        v.assign(VExpr(lp.typ,
          lp.b,
          op match {
            case "<<=" => rtl.ShiftLeft(lp.expr, dist)
            case ">>=" =>
              if (lp.typ.isSigned)
                rtl.ShiftRightArithmetic(lp.expr, dist)
              else
                rtl.ShiftRightLogical(lp.expr, dist)
          })) ++
          v.ref()
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
      rs.foldLeft(l) { case (l, r) =>
        val bl = l.boolExpr
        val br = r.boolExpr
        BExpr(Type.int,
          bl.b ++
            bl.falseTarget ++
            br.b ++
            br.trueTarget ++
            new rtl.Goto(bl.trueTarget.label),
          bl.trueTarget,
          br.falseTarget)
      }
    }

  def logical_and_expr(scope: FScope): Parser[Expr] =
    ior_expr(scope) ~ rep("&&" ~> ior_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, r) =>
        val bl = l.boolExpr
        val br = r.boolExpr
        BExpr(Type.int,
          bl.b ++
            bl.trueTarget ++
            br.b ++
            br.falseTarget ++
            new rtl.Goto(bl.falseTarget.label),
          br.trueTarget,
          bl.falseTarget)
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
    cast_expr(scope)

  def cast_expr(scope: FScope): Parser[Expr] =
    unary_expr(scope) |
      ("(" ~> declSpecs) ~ pos(")") ~ unary_expr(scope) ^^ { case specs ~ lparen ~ e =>
        val typ = DeclSpec.typ(lparen.pos, specs)
        e.convert(typ)
      }

  def unary_expr(scope: FScope): Parser[Expr] =
    ("++" | "--") ~ pos(ident) ^^ { case op ~ id =>
      val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]
      v.assign(
        v.ref().binaryArithOp(Expr.literal(Type.int, 1))((l, r) =>
          rtl.AdditiveExpr(l, r, if (op == "++") "+" else "-"))) ++
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
      (("sizeof" ~ "(") ~> declSpecs) ~ pos(")") ^^ { case specs ~ lparen =>
        val typ = DeclSpec.typ(lparen.pos, specs)
        Expr.literal(Type.int, typ.size)
      } |
      postfix_expr(scope)

  def postfix_expr(scope: FScope): Parser[Expr] =
    (pos(ident) <~ "(") ~ repsep(expr(scope), ",") <~ ")" ^^ { case id ~ args =>
      val f = scope.lookup(id.pos, id.value) match {
        case f: Function => f
        case _ =>
          id.error(s"called object '${id.value}' is not function")
      }
      val retval = f.typ.returnTyp match {
        case TVoid => None
        case _ => Some(Local.gen(f.typ.returnTyp, "retval"))
      }
      val vargs = args.map(_.valueExpr)
      val b = vargs.foldLeft(Block.empty)(_ ++ _.b) ++
        new rtl.Call(f.rtlFunction, retval.map(_.v),
          (f.typ.parameters, vargs).zipped.map {
            case (p, a) =>
              a.convert(p.typ).expr
          }.toIndexedSeq)

      retval match {
        case None => VExpr(TVoid, b, null)
        case Some(v) => b ++ v.ref()
      }
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
        val v = scope.lookup(id.pos, id.value) match {
          case v: Variable => v
          case _ =>
            id.error(s"non-variable '${id.value}' expression scope")
        }
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
      case cond ~ thenStmt ~ elseStmtOpt =>
        val bcond = cond.boolExpr
        elseStmtOpt match {
          case Some(elseStmt) =>
            val Lafter = new rtl.Label(rtl.genLabel())

            bcond.b ++
              bcond.trueTarget ++
              thenStmt ++
              new rtl.Goto(Lafter) ++
              bcond.falseTarget ++
              elseStmt ++
              // fall through
              new rtl.Target(Lafter)

          case None =>
            bcond.b ++
              bcond.trueTarget ++
              thenStmt ++
              // fall through
              bcond.falseTarget
        }
    } |
      ((("while" ~ "(") ~> expr(scope)) <~ ")") ~ stmt(scope) ^^ { case cond ~ stmt =>
        val Lwhile = new rtl.Label(rtl.genLabel())
        val bcond = cond.boolExpr
        Block(new rtl.Target(Lwhile)) ++
          bcond.b ++
          bcond.trueTarget ++
          stmt ++
          new rtl.Goto(Lwhile) ++
          bcond.falseTarget
      } |
      ("do" ~> stmt(scope)) ~ (("while" ~ "(") ~> expr(scope) <~ ")") ^^ { case stmt ~ cond =>
        val Ldo = new rtl.Label(rtl.genLabel())
        val bcond = cond.boolExpr

        Block(new rtl.Target(Ldo)) ++
          stmt ++
          bcond.b ++
          bcond.trueTarget ++
          new rtl.Goto(Ldo) ++
          bcond.falseTarget
      } |
      pos(ident) <~ ":" ^^ { id =>
        val label = scope.functionScope.getLabel(id.value)
        if (label.definitionPos != null) {
          error(
            id.pos.fmt(s"redefinition of label '${id.value}'") +
              label.definitionPos.fmt("previous definition is here", level = "note"))
        }

        label.definitionPos = id.pos

        Block(new rtl.Target(label.rtlLabel))
      } |
      expr(scope) <~ ";" ^^ {
        expr => expr.effect()
      } |
      "goto" ~> pos(ident) <~ ";" ^^ { id =>
        val label = scope.functionScope.getLabel(id.value)
        if (label.usePos == null)
          label.usePos = id.pos

        Block(new rtl.Goto(label.rtlLabel))
      } |
      declSpecs ~ pos(ident) ~ (opt("=" ~> expr(scope)) <~ ";") ^^ { case specs ~ id ~ initopt =>
        val typ = DeclSpec.typ(id.pos, specs)
        if (typ == TVoid)
          id.pos.error(s"variable '${id.value}' may not have 'void' type")

        val v = scope.declareLocal(id.pos, id.value, typ)
        initopt.map(init => v.assign(init)).getOrElse(Block.empty)
      } |
      block_stmt(scope) |
      "return" ~> opt(expr(scope)) <~ ";" ^^ {
        case Some(e) =>
          val c = e.convert(scope.functionScope.function.typ.returnTyp).valueExpr
          c.b ++
            new rtl.Return(Some(c.expr))
        case None =>
          Block(new rtl.Return(None))
      }

  def block_stmt(scope: FScope): Parser[Block] = {
    val bscope = new BlockScope(scope)
    "{" ~> rep(stmt(bscope)) <~ "}" ^^ {
      _.foldLeft(Block.empty)(_ ++ _)
    }
  }

  def param: Parser[Parameter] =
    declSpecs ~ pos(ident) ^^ { case specs ~ id =>
      val typ = DeclSpec.typ(id.pos, specs)
      if (typ == TVoid)
        id.pos.error(s"parameter '${id.value}' may not have 'void' type")

      Parameter(id.pos, id.value, typ)
    }

  def parameters: Parser[IndexedSeq[Parameter]] =
    repsep(param, ",") ^^ {
      _.toIndexedSeq
    }

  def function_head(fileScope: FileScope): Parser[Function] =
    declSpecs ~ pos(ident) ~ ("(" ~> parameters <~ ")") ^^ { case specs ~ id ~ params =>
      val returnTyp = DeclSpec.typ(id.pos, specs)
      fileScope.declareFunction(id.pos, id.value, TFunction(returnTyp, params),
        DeclSpec.isNonstd(specs))
    }

  def function_body(fileScope: FileScope, f: Function): Parser[Block] = {
    val fscope = f.functionScope(fileScope)
    "{" ~> rep(stmt(fscope)) <~ "}" ^^ { stmts =>
      fscope.labels.values.foreach { label =>
        if (label.definitionPos == null)
          label.usePos.error(s"use of undeclared label '${label.id}'")
      }

      stmts.foldLeft(Block.empty)(_ ++ _)
    }
  }

  def function(fileScope: FileScope): Parser[Function] =
    function_head(fileScope)
      .flatMap { f =>
        (";" |
          function_body(fileScope, f) ^^ { b =>
            f.rtlFunction.body = Some(rtl.FunctionBody(b.stmts))
          }) ^^ { _ => f }
      }

  def compilation_unit: Parser[rtl.CompilationUnit] = {
    val fileScope = new FileScope
    rep(function(fileScope)) ^^ { _ =>
      rtl.CompilationUnit(fileScope.decls.values
        .map(_.asInstanceOf[Function].rtlFunction)
        .toSeq)
    }
  }
}
