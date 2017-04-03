package cs.cynth

// TODO:
// write write_uart in Verilog
// reset call regs
// don't check idle in call
// test example

// rtl.Expr.emit => toVerilog: String
// hex integer literals
// fall off bottom
// character literals

// remove redundant states (don't emit label, goto, make next state more sophisticated)
// implement emit on missing operators
// mux

// define function extern or not (std)

// memory

object Main {

  def main(args: Array[String]): Unit = {
    if (false) {
      val cu = rtl.CompilationUnit.example()
      cu.emit()
    }

    if (true) {
      val text = """int f(int x) {
  int x;
}
"""

      // val cu = c.Parser.parseString(text)
      val cu = c.Parser.parseFile("src/test/resources/valid/alphabet-goto.c")
      cu.check()

      cu.pretty()

      cu.emit()
    }
  }
}
