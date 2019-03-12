import org.junit.Test
import org.junit.Assert._

import Macro._

/**
  * These tests test all the possible formats the f interpolator has to deal with.
  * The tests are sorted by argument category as the arguments are on https://docs.oracle.com/javase/6/docs/api/java/util/Formatter.html#detail
  *
  *
  * Some also test (briefly) the three other macros that are implemented ; namely the s, the raw and the foo interpolators.
  */
class PositiveTest {

  @Test def generalArgsTests() = {  

    def booleanTest(b : Boolean) = f2"The boolean is $b%b" 

    def hTest(arg : Int) = f2"$arg%h"

    def stringTest(s : String) = f2"The string is $s%s"

    def noFormatHasStringDefault(s : String) = f2"The string is $s"

    assertEquals("The boolean is false", booleanTest(false))
    assertEquals("The boolean is true", booleanTest(true))

    val arg : Int = 10
    val expected = Integer.toHexString(arg.hashCode)
    assertEquals(expected.toString, hTest(arg))

    assertEquals("The string is null", stringTest(null))
    assertEquals("The string is ", stringTest(""))
    assertEquals("The string is string1", stringTest("string1"))

    assertEquals("The string is null", noFormatHasStringDefault(null))
    assertEquals("The string is ", noFormatHasStringDefault(""))
    assertEquals("The string is string2", noFormatHasStringDefault("string2"))
  }

  @Test def characterArgsTests() = {

    def charTest(c : Char) = f2"The unicode character is $c%c"

    assertEquals("The unicode character is c", charTest('c'))
  }

  @Test def integralArgsTests() = {

    def decimalIntegerTest(i : Int) = {
      f2"The decimal integer is $i%d, $i%2d, $i%d+1 = ${i+1}%d"
    }

    def octalIntegerTest(i : Int) = {
      f2"The octal integer is $i%o"
    }

    def hexadecimalIntegerTest(i : Int) = {
      f2"The hexadecimal integer is $i%x"
    }

    assertEquals("The decimal integer is 2,  2, 2+1 = 3",
      decimalIntegerTest(2))

    //TODO : decimalIntegerTest("hello") //TODO : does not compile -- not an exception do throw exception inside macro and catch it here 
    //TODO : IllegalFormatConversionException 

    assertEquals("The octal integer is 2", octalIntegerTest(2))
    assertEquals("The octal integer is 10", octalIntegerTest(8))

    assertEquals("The hexadecimal integer is 2", hexadecimalIntegerTest(2))
    assertEquals("The hexadecimal integer is 20", hexadecimalIntegerTest(32))
  }

  @Test def floatingPointArgsTests() = {
    def scientificNotationTest(f : Float) = f2"The scientific notation is $f%e"

    def decimalFloatingPointTest(f : Float) = {
      f2"The decimal floating point is $f%f, $f%3.2f, $f%15.3f"
    } 

    def noPointFloatingPointTest(f : Float) = {
      f2"The decimal floating point is $f%2.0f"
    }

    def gTest(f : Float) = f2"The float value is $f%g"

    def aTest(f : Float) = f2"The float value is $f%a"

    assertEquals("The scientific notation is 2.000000e+00", scientificNotationTest(2f))
    assertEquals("The scientific notation is 5.430000e-01", scientificNotationTest(0.543f))

    assertEquals("The decimal floating point is 1234.567749, 1234.57,        1234.568", decimalFloatingPointTest(1234.5678f))

    assertEquals("The decimal floating point is 10", noPointFloatingPointTest(10.0f))

    val i = Math.pow(10.0, -4).floatValue()

    assertEquals("The float value is 0.000100000", gTest(i))
    assertEquals("The float value is 1.00000e-05", gTest(i/10.0f))

    assertEquals("The float value is -0x1.4f8b58p-17", aTest(-i/10.0f))
    assertEquals("The float value is 0x0.0p0", aTest(+0f))
    assertEquals("The float value is NaN", aTest(Float.NaN))
    assertEquals("The float value is Infinity", aTest(Float.PositiveInfinity))
    assertEquals("The float value is -Infinity", aTest(Float.NegativeInfinity))
  }

  //TODO : @Test def dateArgsTests() = ??? 

  @Test def specificLiteralsTests() = {
    def percentArgsTest() = {
      f2"the percentage is 100 %%"
    }

    def lineSeparatorArgs() = {
      f2"we have a line separator now %%n and now, we are on the next line"
    }

    assertEquals("the percentage is 100 %", percentArgsTest())
    assertEquals("we have a line separator now %n and now, we are on the next line", lineSeparatorArgs())
  }
}
