import Macro._

object Test2 {
  def main(args: Array[String]): Unit = {
    val w = "world"
    println(s2"Hello $w!")
    println(raw2"Hello $w!\n")
    println(foo"Hello $w!")
  }
}
