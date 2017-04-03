package cs.cynth

package object rtl {
  private var counter: Int = 0

  def genLabel(): String = {
    counter += 1

    s"_L${counter - 1}"
  }

  def gensym(root: String): String = {
    counter += 1
    s"__${root}_gen${counter - 1}"
  }
}
