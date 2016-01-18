/**
 * Created by lkhamsurenl on 1/18/16.
 */

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class Chapter6Test extends AssertionsForJUnit {
  // Repeatedly generate the random number to see all of them are non-negative.
  @Test def nonNegativeIntTest(): Unit = {
    var rng = RNG.Simple(2525L)
    for {_ <- 1 to 100} {
      val (value, rng2: RNG.Simple) = RNG.nonNegativeInt(rng)
      assert(value >= 0)
      rng = rng2
    }
  }

  // Repeatedly generate the random number to see all of them are double [0, 1).
  @Test def doubleTest(): Unit = {
    var rng = RNG.Simple(2525L)
    for {_ <- 1 to 100} {
      val (value, rng2: RNG.Simple) = RNG.double(rng)
      assert(0.0 <= value && value < 1.0, s"${value}; want: 0 <= x < 1")
      rng = rng2
    }
  }
}
