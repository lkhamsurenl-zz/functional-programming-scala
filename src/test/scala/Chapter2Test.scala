/**
 * Created by lkhamsurenl on 1/10/16.
 */

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Chapter2Test extends AssertionsForJUnit {
  @Test def findFirstTest(): Unit = {
    val got1 = Chapter2.findFirst(List(1,2,3),(x:Int) => x >= 2)
    assert(got1 == 1, s"findFirst(List(1,2,3)) = ${got1}; want: 2")

    val got2 = Chapter2.findFirst(List(1.0,2.0,3.0),(x:Double) => x >= 3.0)
    assert(got2 == 2, s"findFirst(List(1.0,2.0,3.0)) = ${got2}; want: 2")
  }
}
