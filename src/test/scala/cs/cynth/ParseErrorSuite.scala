package cs.cynth

import java.io.{FileOutputStream, PrintStream}

import org.scalatest.FunSuite

import scala.io.Source
import scala.sys.process._
import scala.language.postfixOps

class ParseErrorSuite extends FunSuite {

  val runFiles = Seq(
    "const.c",
    "loop.c"
  )

  test("run") {
    runFiles.foreach { f =>
      val p = "src/test/resources/run/" + f

      val line1 = Source.fromFile(p).getLines.next
      assert(line1.startsWith("// expected: "))
      val expected = line1.drop(13)
      println(s"expected = $expected")

      val cu = c.Parser.parseFile(p)
      cu.check()

      val fOut = new PrintStream(new FileOutputStream("f.v"))
      fOut.println("`timescale 1 ns / 1 ps")
      fOut.println("`default_nettype none")
      fOut.println()

      cu.emit(fOut)
      fOut.close()

      val topOut = new PrintStream(new FileOutputStream("top.v"))
      topOut.println("`timescale 1 ns / 1 ps")
      topOut.println("`default_nettype none")
      topOut.println()
      topOut.println("module top();")
      topOut.println(s"  tb #(.expected($expected)) tb_inst();")
      topOut.println("endmodule")

      val compileExitCode = "/usr/local/iverilog/bin/iverilog -s top -o top f.v tb.v top.v" !;
      assert(compileExitCode == 0)

      val simExitCode = "/usr/local/iverilog/bin/vvp top" !;
      assert(simExitCode == 0)
    }
  }

  val validFiles = Seq(
    "alphabet-goto.c",
    "empty-body.c",
    "no-return.c",
    "hex-literal.c",
    "char-literal.c",
    "assign.c",
    "uart.c"
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
    "incomplete-function.c",
    "label-redefinition.c",
    "call-var.c",
    "fun-expr.c")

  test("parse error") {
    parseErrorFiles.foreach { f =>
      assertThrows[c.ParseError] {
        c.Parser.parseFile("src/test/resources/parse-error/" + f)
      }
    }
  }
}
