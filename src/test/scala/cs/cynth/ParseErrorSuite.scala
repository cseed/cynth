package cs.cynth

import org.scalatest.FunSuite

class ParseErrorSuite extends FunSuite {

  val validFiles = Seq(
    "alphabet-goto.c",
    "empty-body.c",
    "no-return.c",
    "hex-literal.c"
  )

  test("valid") {
    validFiles.foreach { f =>
      val cu = c.Parser.parseFile("src/test/resources/valid/" + f)
      cu.check()
    }
  }

  val parseErrorFiles = Seq(
    "void-param.c",
    "void-variable.c",
    "incomplete-function.c")

  test("parse error") {
    parseErrorFiles.foreach { f =>
      assertThrows[c.ParseError] {
        c.Parser.parseFile("src/test/resources/parse-error/" + f)
      }
    }
  }
}
