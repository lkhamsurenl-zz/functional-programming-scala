import scala.annotation.tailrec

/**
 * Created by luvsandondov on 8/15/15.
 */
// Random number generator that also returns the next state s.t. the caller has the control over what to do next with
// the next state.
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // generating next random integer
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = Simple(newSeed)
      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)
    }
  }

  // Get 2 random pairs
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (v1, rng1) = rng.nextInt
    val (v2, rng2) = rng1.nextInt
    ((v1, v2), rng2)
  }

  // Return random integer btw 0 to Int.maxValue (inclusive).
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    if (v != Int.MinValue)
      (math.abs(v), rng2)
    else
      (0, rng2)
  }

  // Generate a random # between 0 and 1 (not inclusive).
  def double(rng: RNG): (Double, RNG) = {
    val (v1, rng1) = nonNegativeInt(rng)
    if (v1 == Int.MaxValue)
      (0.0, rng1)
    else
      (v1.toDouble / Int.MaxValue, rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  // Generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def recInts(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) (acc, rng)
      else {
        val (value, rng1) = rng.nextInt
        recInts(count - 1, rng1, value::acc)
      }
    }
    recInts(count, rng, Nil)
  }
}

// Type Rand encapsulates function type that takes RNG and returns (A, RNG)
type Rand[+A] = RNG => (A, RNG)
object Rand {
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](a: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (value, rng1) = a(rng)
      (f(value), rng1)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(RNG.nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(RNG.nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def int: Rand[Int] = _.nextInt

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]()))((rl, ra) => map2(ra, rl)(_::_))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def mapByFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
