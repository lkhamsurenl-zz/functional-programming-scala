import scala.annotation.tailrec

/**
 * Created by luvsandondov on 8/15/15.
 */
trait RNG {
  def nextInt: (Int, RNG)
  
}

object RNG {
  // generating next random integer
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }
  
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Int.MaxValue + 1, r)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  } 
  def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {case ((i, d), r) => ((d, i), r)}
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def tailInts(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) =
      if (count == 0) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        tailInts(count - 1, i::acc)(r)
      }
    tailInts(count, Nil)(rng)
  }

  
}
