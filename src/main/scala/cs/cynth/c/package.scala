package cs.cynth

import scala.util.parsing.input.Position
import scala.language.implicitConversions

package object c {
  implicit def toRichPosition(p: Position): RichPosition = new RichPosition(p)
}
