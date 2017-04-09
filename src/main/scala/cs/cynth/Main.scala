package cs.cynth

object Main {

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println(s"usage: cynth <c-file>")
      System.exit(1)
    }

    try {
      val cu = c.Parser.parseFile(args(0))
      cu.check()
      cu.emit(System.out)
    } catch {
      case e: c.ParseError =>
        System.err.println(e.getMessage)
    }
  }
}
