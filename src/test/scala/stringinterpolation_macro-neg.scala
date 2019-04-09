
import Macro._

/**
  * These tests test some combinations that should make the f interpolator fail. 
  * They come from https://github.com/lampepfl/dotty/blob/master/tests/untried/neg/stringinterpolation_macro-neg.scala 
  * The expected result is in the .check file (only need to modify "f" by "f2" in that file)
  */

object Test extends App {
  val s = "Scala"
  val d = 8 
  val b = false
  val f = 3.14159   
  val c = 'c'
  val t = new java.util.Date
  val x = new java.util.Formattable {
    def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff format "xxx"
  }

  // 1) number of arguments
  // new StringContext().f2() 
  // StringContext().f2() 
  // new StringContext("", " is ", "%2d years old").f2(s)
  // new StringContext("", " is ", "%2d years old").f2(s, d, d)
  // new StringContext("", "").f2() 

  // 2) Interpolation mismatches
  // f2"$s%b"
  // f2"$s%c"
  // f2"$f%c"
  // f2"$s%x"
  // f2"$b%d"
  // f2"$s%d"
  // f2"$f%o"
  // f2"$s%e" 
  // f2"$b%f"

  // {
  //   implicit val strToInt1 = (s: String) => 1
  //   implicit val strToInt2 = (s: String) => 2
  //   f2"$s%d"
  // }

  // f2"$s%i"  

  // 3) flag mismatches 
  // f2"$s%+ 0,(s"  
  // f2"$c%#+ 0,(c"
  // f2"$d%#d"
  // f2"$d%,x"
  // f2"$d%+ (x"
  // f2"$f%,(a"
  // f2"$t%#+ 0,(tT"
  
  // 4) bad precisions 
  // f2"$c%.2c"
  // f2"$d%.2d" 
  // f2"%.2%"
  // f2"%.2n" 
  // f2"$f%.2a"
  // f2"$t%.2tT"

  // 5) bad indexes 
  // f2"%<s" 
  // f2"%<c"
  // f2"%<tT"
  // f2"%1$$n" 
  // f2"${8}%d ${9}%d %3$$d"  
  
  // f2"${8}%d ${9}%d%0$$d"

  // warnings 
  // f2"${8}%d ${9}%1$$d"
  // f2"$s%s $s%s %1$$<s"
  // f2"$s%s $s%1$$s"

  // 6) bad arg types 
  // f2"$s%#s"

  // 7) misunderstood conversions 
  // f2"$t%tG" 
  // f2"$t%t"
  // f2"$s%10.5"

  // 8) other brain failures //TODO next time
  // f2"${d}random-leading-junk%d" //TODO : look for %, if shit inside, return error part[1] = random-leading-junk%d
}