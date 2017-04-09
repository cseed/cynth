package cs.cynth

import java.io._

import scala.sys.process._
import scala.language.postfixOps

// TODO:
// write write_uart in Verilog
// reset call regs
// don't check idle in call
// test example

// fix FIXMEs
// check for label with no definition

// statements: for

// remove redundant states (don't emit label, goto, make next state more sophisticated)

// define function extern or not (std)

// memory

object Main {

  def main(args: Array[String]): Unit = {
    val text = """
// expected: 32'd10
int f() {
  int i = 0;
  int s = 0;
  while (i < 5) {
    s += i;
    ++i;
  }
  return s;
}
"""

    // val cu = c.Parser.parseString(text)

    val cu = c.Parser.parseFile("src/test/resources/parse-error/label-redefinition.c")
    cu.check()

    cu.pretty()

    cu.emit(System.out)
  }
}
