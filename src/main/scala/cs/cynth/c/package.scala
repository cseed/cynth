package cs.cynth

import scala.util.parsing.input.Position

package object c {
  type Stmts = IndexedSeq[rtl.Statement]

  implicit def toRichPosition(p: Position): RichPosition = new RichPosition(p)
}
