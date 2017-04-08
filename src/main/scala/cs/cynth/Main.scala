package cs.cynth

import java.io.{File, FileWriter}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// TODO:
// write write_uart in Verilog
// reset call regs
// don't check idle in call
// test example

// make test benches for examples

// statements: for

// remove redundant states (don't emit label, goto, make next state more sophisticated)

// define function extern or not (std)

// memory

object Main {

  def main(args: Array[String]): Unit = {
    val text = """int f() {
  int i = 0;
  int s = 0;
  while (i < 3) {
    s += i;
    ++i;
  }
  return s;
}
"""
    val cu = c.Parser.parseString(text)

    // val cu = c.Parser.parseFile("src/test/resources/valid/uart.c")
    cu.check()

    cu.pretty()

    cu.emit()
  }
}
