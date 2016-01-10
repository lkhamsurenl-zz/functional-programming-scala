import scala.annotation.tailrec

/**
 * Created by luvsandondov on 8/9/15.
 */
object Chapter2 {
  // Find a first occurence that satisfies predicate.
  def findFirst[A](lst: List[A], p: A => Boolean): Int = {
    @tailrec
    def loop(xs: List[A], i: Int): Int = xs match {
      case l::ls => if (p(l)) i else loop(ls, i + 1)
      case nil   => -1
    }

    loop(lst, 0)
  }

  // function checks if the array of generic type is sorted
  def isSorted[A](as: List[A], ordered: (A,A) => Boolean): Boolean = {
    as match {
      case List() | List(_) => true
      case a::b::cs => ordered(a,b) && isSorted(b::cs, ordered)
    }
  }

  // Partially applies the function by passing one of the parameters.
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a,b)

  // Curry using partial1
  // Convert function that takes 2 arguments to function that returns function, which takes an argument and returns
  // a value.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)
  
  // uncurry the above, A => B => C === A => (B => C) holds because of the right associativity
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // Compose function f and g.
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}

object MyModule {
  // Find factorial of a given integer.
  def factorial(n: Int): Int = {
    @tailrec
    def factorialTail(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else factorialTail(n - 1, n * acc)
    }
    factorialTail(n, 1)
  }
  
  // Tail rec function to compute fibonnaci number n
  def fibonnaci(n: Int): Int = {
    @tailrec
    def inFib(n: Int, sm: Int, la: Int): Int = {
      if (n <= 1) la
      else inFib(n - 1, la, sm + la)
    }
    inFib(n, 0, 1)
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(Chapter2.isSorted(1::2::3::Nil, (a: Int, b: Int) => a < b))
  }
}
