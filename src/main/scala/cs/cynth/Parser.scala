package cs.cynth

case class Type(size: Int)

sealed abstract class Expr

// FIXME BigInteger
case class Literal(typ: Type, value: Int)

case class Add(lhs: Expr, rhs: Expr)

case class Sub(lhs: Expr, rhs: Expr)

case class Neg(expr: Expr)

case class ShiftLeft(lhs: Expr, rhs: Expr)

case class ShiftRightLogical(lhs: Expr, rhs: Expr)

case class ShiftRightArithmetic(lhs: Expr, rhs: Expr)

case class Eq(lhs: Expr, rhs: Expr)

case class Not(expr: Expr)

case class Lt(lhs: Expr, rhs: Expr)

case class ULt(lhs: Expr, rhs: Expr)

case class SignExtend(typ: Type, expr: Expr)

case class ZeroExtend(typ: Type, expr: Expr)

case class Truncate(typ: Type, expr: Expr)

case class Local(id: String, typ: Type)

case class Parameter(id: String, typ: Type)

sealed abstract class Statement

case class Assign(id: String, expr: Expr) extends Statement

case class Goto(bb: BasicBlock) extends Statement

case class Branch(cond: Expr, thenBB: BasicBlock, elseBB: BasicBlock) extends Statement

// FIXME read/write array

// FIXME return value
case class Call(id: String, arguments: IndexedSeq[Expr]) extends Statement

case class Return(expr: Expr) extends Statement

case class BasicBlock(stmts: IndexedSeq[Statement])

case class Function(returnTyp: Type,
  parameters: IndexedSeq[Parameter],
  locals: IndexedSeq[Local],
  bbs: IndexedSeq[BasicBlock])

/*
import scala.util.parsing.combinator.RegexParsers

class CynthParser extends RegexParsers {
  def typ: Parser[Type] =
    "int[0-9]+".r ^^ { s => TInt(s.drop(3).toInt) } |
      "uint[0-9]+".r ^^ { s => TUInt(s.drop(4).toInt) } |
      "int" ^^ { _ => TInt(32) } |
      "uint" ^^ { _ => TUInt(32) }

  def ident: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r

  def param_decl: Parser[Parameter] =
    typ ~ ident ^^ { case typ ~ id => Parameter(id, typ) }

  def local_decl: Parser[Local] =
    typ ~ ident ^^ { case typ ~ id => Local(id, typ) }


} */
