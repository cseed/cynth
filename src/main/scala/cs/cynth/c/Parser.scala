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

sealed abstract class Type {
  def size: Int

  def integerPromotion(): Type = {
    if (size < 32)
      TInt(32)
    else
      this
  }

  def arithmeticConversion(other: Type): Type = {
    if (size > other.size)
      this
    else if (other.size > size)
      other
    else if (isInstanceOf[TUInt])
      this
    else
      other
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

// FIXME Value, Bool exprs
case class Expr(typ: Type,
                stmts: Stmts,
                expr: rtl.Expr) {
  def convert(newTyp: Type): Expr = {
    Expr(newTyp,
      stmts,
      if (newTyp.size == typ.size)
        expr
      else if (newTyp.size < typ.size)
        rtl.Truncate(newTyp.size, expr)
      else
        (typ: @unchecked) match {
          case TInt(_) =>
            rtl.SignExtend(newTyp.size, expr)
          case TUInt(_) =>
            rtl.ZeroExtend(newTyp.size, expr)
          // FIXME check
        })
  }

  def integerPromotion(): Expr = convert(typ.integerPromotion())

  def arithmeticConversion(other: Expr): (Expr, Expr) = {
    val p = integerPromotion()
    val pother = other.integerPromotion()

    val newTyp = p.typ.arithmeticConversion(pother.typ)
    (p.convert(newTyp), pother.convert(newTyp))
  }
}

sealed abstract class Decl {
  def pos: Position

  def id: String

  def typ: Type
}

abstract class Variable extends Decl {
  def v: rtl.Variable
}

case class Local(pos: Position, id: String, typ: Type) extends Variable {
  def v = new rtl.Local(id, typ.size)
}

case class Parameter(pos: Position, id: String, typ: Type) extends Variable {
  def v = new rtl.Parameter(id, typ.size)
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
      "int" ^^ { _ => TInt(32) } |
      "unsigned" ^^ { _ => TUInt(32) } |
      // FIXME _Bool type?
      "_Bool" ^^ { _ => TUInt(1) } |
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
      Expr(v.typ,
        e.stmts ++
          IndexedSeq(new rtl.Assign(v.v, e.convert(v.typ).expr)),
        rtl.Ref(v.v))
    } |
      cond_expr(scope)

  def cond_expr(scope: FScope): Parser[Expr] =
    (ior_expr(scope) <~ "?") ~ (expr(scope) <~ ":") ~ cond_expr(scope) ^^ { case c ~ t ~ f =>
      val (pt, pf) = t.arithmeticConversion(f)
      Expr(pt.typ,
        c.stmts ++ pt.stmts ++ pf.stmts,
        rtl.Mux(
          rtl.RelationalExpr(c.expr, rtl.Literal(c.typ.size, 0), "!=", isSigned = false),
          pt.expr,
          pf.expr))
    } |
      ior_expr(scope)

  def ior_expr(scope: FScope): Parser[Expr] =
    xor_expr(scope) ~ rep("|" ~ xor_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        Expr(pl.typ,
          pl.stmts ++ pr.stmts,
          rtl.BinaryLogicalExpr(pl.expr, pr.expr, "|"))
      }
    }

  def xor_expr(scope: FScope): Parser[Expr] =
    and_expr(scope) ~ rep("^" ~ and_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        Expr(pl.typ,
          pl.stmts ++ pr.stmts,
          rtl.BinaryLogicalExpr(pl.expr, pr.expr, "^"))
      }
    }

  def and_expr(scope: FScope): Parser[Expr] =
    equality_expr(scope) ~ rep("&" ~ equality_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        Expr(pl.typ,
          pl.stmts ++ pr.stmts,
          rtl.BinaryLogicalExpr(pl.expr, pr.expr, "&"))
      }
    }

  def equality_expr(scope: FScope): Parser[Expr] =
    relational_expr(scope) ~ rep(("==" | "!=") ~ relational_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        Expr(TInt(32),
          pl.stmts ++ pr.stmts,
          rtl.ZeroExtend(32,
            rtl.RelationalExpr(pl.expr, pr.expr, op, pl.typ.isSigned)))
      }
    }

  def relational_expr(scope: FScope): Parser[Expr] =
    shift_expr(scope) ~ rep(("<=" | "<" | ">=" | ">") ~ shift_expr(scope)) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (pl, pr) = l.arithmeticConversion(r)
        Expr(TInt(32),
          pl.stmts ++ pr.stmts,
          rtl.ZeroExtend(32,
            rtl.RelationalExpr(pl.expr, pr.expr, op, pl.typ.isSigned)))
      }
    }

  def shift_expr(scope: FScope): Parser[Expr] =
    additive_expr(scope) ~ rep(("<<" | ">>") ~ int_literal) ^^ { case l ~ rs =>
      rs.foldLeft(l) { case (l, op ~ r) =>
        val (_, dist) = r
        val lp = l.integerPromotion()
        Expr(lp.typ,
          lp.stmts,
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
        Expr(pl.typ,
          pl.stmts ++ pr.stmts,
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
    ("++" | "--") ~ unary_expr(scope: FScope) ^^ { case op ~ e =>
      // FIXME
      ???
    } |
      ("-" | "+" | "~" | "!") ~ cast_expr(scope) ^^ { case op ~ e =>
        val p = e.integerPromotion()
        op match {
          case "-" => Expr(p.typ, p.stmts, rtl.Neg(p.expr))
          case "+" => p
          case "~" => Expr(p.typ, p.stmts, rtl.Not(p.expr))
          case "!" => Expr(TInt(32), p.stmts,
            rtl.ZeroExtend(32,
              rtl.RelationalExpr(p.expr, rtl.Literal(p.typ.size, 0), "==", isSigned = false)))
        }
      } |
      "sizeof" ~> expr(scope) ^^ { e =>
        Expr(TInt(32), IndexedSeq(), rtl.Literal(32, e.typ.size))
      } |
      (("sizeof" ~ "(") ~> typ) <~ ")" ^^ { typ =>
        Expr(TInt(32), IndexedSeq(), rtl.Literal(32, typ.size))
      } |
      postfix_expr(scope)

  def postfix_expr(scope: FScope): Parser[Expr] =
    (pos(ident) <~ "(") ~ repsep(expr(scope), ",") <~ ")" ^^ {
      case id ~ args =>
        val f = scope.lookup(id.pos, id.value).asInstanceOf[Function]
        // FIXME void
        // FIXME gensym
        val retval = new rtl.Local(rtl.gensym("retval"), f.typ.returnTyp.size)
        Expr(f.typ.returnTyp,
          args.flatMap(e => e.stmts).toIndexedSeq :+
            new rtl.Call(f.rtlFunction, Some(retval),
              (f.typ.parameters, args).zipped.map {
                case (p, a) =>
                  a.convert(p.typ).expr
              }.toIndexedSeq),
          rtl.Ref(retval))
    } | pos(ident) ~ ("++" | "--") ^^ { case id ~ op =>
      val v = scope.lookup(id.pos, id.value).asInstanceOf[Variable]

      val ve = Expr(v.typ, IndexedSeq(), rtl.Ref(v.v))
      val one = Expr(TInt(32), IndexedSeq(), rtl.Literal(32, 1))
      val (vep, onep) = ve.arithmeticConversion(one)
      val sum = Expr(
        vep.typ,
        vep.stmts ++ onep.stmts,
        rtl.AdditiveExpr(vep.expr, onep.expr,
          if (op == "++") "+" else "-"))

      val t = new rtl.Local(rtl.gensym("postfix"), v.typ.size)

      Expr(v.typ,
        (IndexedSeq(new rtl.Assign(t, rtl.Ref(v.v))) ++ one.stmts) :+
          new rtl.Assign(v.v, sum.convert(v.typ).expr),
        rtl.Ref(v.v))
    } |
      primary_expr(scope)

  def primary_expr(scope: FScope): Parser[Expr] =
    "(" ~> expr(scope) <~ ")" |
      pos(ident) ^^ { id =>
        // FIXME check
        val decl = scope.lookup(id.pos, id.value)
        val v = decl.asInstanceOf[Variable]
        Expr(v.typ, IndexedSeq(), rtl.Ref(v.v))
      } |
      int_literal ^^ { case (size, i) =>
        Expr(TInt(size), IndexedSeq(), rtl.Literal(size, i))
      }

  def int_literal: Parser[(Int, Int)] =
    "0x[0-9a-fA-F]+".r ^^ { s =>
      (32, java.lang.Integer.parseInt(s.drop(2), 16))
    } |
      "[0-9]+".r ^^ { s => (32, s.toInt) } |
      "'[^'\\\\\n]'".r ^^ { s => (8, s.apply(1).toInt) } |
      "'\\['\\\\\"?abfnrtv]'".r ^^ { s =>
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

  def stmt(scope: FScope): Parser[Stmts] =
    ("if" ~> "(" ~> expr(scope) <~ ")") ~ stmt(scope) ~ opt("else" ~> stmt(scope)) ^^ {
      case cond ~ thenStmt ~ elseStmt =>
        // FIXME specialize elseStmt == None
        val Lthen = new rtl.Label(rtl.genLabel())
        val Lelse = new rtl.Label(rtl.genLabel())
        val Lafter = new rtl.Label(rtl.genLabel())

        cond.stmts ++
          IndexedSeq(
            new rtl.Branch(rtl.RelationalExpr(cond.expr, rtl.Literal(cond.typ.size, 0), "!=", isSigned = false),
              Lthen, Lelse),
            new rtl.Target(Lthen)) ++
          thenStmt ++
          IndexedSeq(new rtl.Goto(Lafter),
            new rtl.Target(Lelse)) ++
          elseStmt.getOrElse(IndexedSeq.empty) ++
          IndexedSeq(
            new rtl.Goto(Lafter),
            new rtl.Target(Lafter))
    } |
      ident <~ ":" ^^ { id =>
        // FIXME check
        val label = scope.functionScope.getLabel(id)
        IndexedSeq(new rtl.Target(label.rtlLabel))
      } |
      expr(scope) <~ ";" ^^ {
        expr => expr.stmts
      } |
      "goto" ~> ident <~ ";" ^^ { id =>
        // FIXME check
        val label = scope.functionScope.getLabel(id)
        IndexedSeq(new rtl.Goto(label.rtlLabel))
      } |
      // FIXME intializer
      typ ~ pos(ident) <~ ";" ^^ { case typ ~ id =>
        if (typ == TVoid)
          id.pos.parseError(s"variable '${
            id.value
          }' may not have 'void' type")

        scope.declareLocal(id.pos, id.value, typ)
        IndexedSeq()
      } |
      block_stmt(scope) |
      "return" ~> opt(expr(scope)) <~ ";" ^^ {
        case Some(e) =>
          val c = e.convert(scope.functionScope.function.typ.returnTyp)
          e.stmts ++
            IndexedSeq(new rtl.Return(Some(e.expr)))
        case None =>
          IndexedSeq(new rtl.Return(None))
      }

  def block_stmt(scope: FScope): Parser[Stmts] = {
    val bscope = new BlockScope(scope)
    "{" ~> rep(stmt(bscope)) <~ "}" ^^ {
      _.toIndexedSeq.flatten
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

  def function_body(fileScope: FileScope, f: Function): Parser[Stmts] = {
    val fscope = f.functionScope(fileScope)
    "{" ~> rep(stmt(fscope)) <~ "}" ^^ {
      _.toIndexedSeq.flatten
    }
  }

  def function(fileScope: FileScope): Parser[Function] =
    function_head(fileScope)
      .flatMap { f =>
        (";" |
          function_body(fileScope, f) ^^ {
            stmts =>
              f.rtlFunction.body = Some(rtl.FunctionBody(
                IndexedSeq(),
                stmts))
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