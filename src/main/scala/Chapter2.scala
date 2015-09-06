import scala.annotation.tailrec

/**
 * Created by luvsandondov on 8/9/15.
 */
class Chapter2 {
  // function checks if the array of generic type is sorted
  def isSorted[A](as: List[A], ordered: (A,A) => Boolean): Boolean = {
    as match {
      case List() | List(_) => true
      case a::b::cs => ordered(a,b) && isSorted(b::cs, ordered)
    }
  }
  // partially applies the function
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = 
    (b: B) => f(a, b)
  // curry using partial1
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
    (a: A) => partial1(a, f)
  
  // uncurry the above, A => B => C === A => (B => C) holds because of the right associativity
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
  
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}

object MyModule {
  // factorial of an number 
  def factorial(n: Int): Int = {
    @tailrec
    def factorialTail(n: Int, coll: Int): Int = {
      if (n <= 1) coll
      else factorialTail(n - 1, n * coll)
    }
    factorialTail(n, 1)
  }
  
  // tail rec function to compute fibonnaci number n
  def fibonnaci(n: Int): Int = {
    @tailrec
    def inFib(n: Int, sm: Int, la: Int): Int = {
      if (n <= 1) la
      else inFib(n - 1, la, sm + la)
    }
    inFib(n, 0, 1)
  }

  def abs(n: Int): Int = 
    if (n < 0) -n
    else n
  
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  // main is procedure , because it has a side effects
  def main(args: Array[String]): Unit = {
    val c = new Chapter2()
    println(c.isSorted(1::2::3::Nil, (a: Int, b: Int) => a < b))
  }
}
