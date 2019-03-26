import org.junit.Test
import org.junit.Assert._

import compilationAssertions._

/**
  * These tests test all the possible formats the f interpolator has to deal with.
  * The tests are sorted by argument category as the arguments are on https://docs.oracle.com/javase/6/docs/api/java/util/Formatter.html#detail
  *
  *
  * Some also test (briefly) the three other macros that are implemented ; namely the s, the raw and the foo interpolators.
  */
class NegativeTests {


    @Test def generalArgsTests() = {  
      assertNotCompile("StringContext().f2()") //TODO : add nexts
      
    }
    @Test def generalArgsTests1() = {  
      assertNotCompile("StringContext().f2()") //TODO : add nexts - all per line per test 
      
    }
}