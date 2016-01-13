import scala.annotation.tailrec

/**
 * Created by luvsandondov on 8/13/15.
 */
object Chapter5 {
  // two function does not get evaluated until you call it
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse
  
  def if3[A](cond: Boolean, onTrue: () => A, onFalse: () => A) : A = 
    if (cond) onTrue() else onFalse()
  
  
  def main(args: Array[String]): Unit = {
    if2(1 < 22, () => print("a"), () => print("b"))
    
  }
}

// Empty stream.
case object Empty extends Stream[Nothing]
// Non-empty stream. It has an explicit thunk to ensure the non-explicit
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// +A means Dog <: Animal -> Stream[Dog] <: Stream[Animal]
sealed trait Stream[+A] {
  // Return head if exist
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(head, tail) => Some(head())
  }

  // Convert to list with tail recursive function
  def toList: List[A] = {
    @tailrec
    def inList(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, tl) => inList(tl(), h()::acc)
    }
    // do not forget to reverse the list
    inList(this, List[A]()).reverse
  }

  // Take first elements in stream
  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 0 => Cons(head, () => tail().take(n-1))
    case _ => Empty
  }

  // drop first n elements of a stream
  def drop(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (l, 0) => l
    case (Cons(h, tl), num) => tl().drop(num - 1)
  }

  // it is lazy function since 2nd part of || would not evaluate if first is true
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, tl) => p(h()) || tl().exists(p)
    case _ => false
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
    case _ => z
  }
  
  def forAll(f: A => Boolean): Boolean = this.exists(x => !f(x))

}


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd // cache the value to avoid repeated computation
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  
  // infinite stream
  def constant[A](value: A): Stream[A] = cons(value, constant(value))
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  
  def fibs(): Stream[Int] = {
    // generate infinite stream from two values
    def nFib(f0: Int, f1: Int): Stream[Int] = cons(f0, nFib(f1, f0 + f1))
    nFib(0, 1)
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def onesByUnfold(): Stream[Int] = unfold(1)(i => Some((1,1)))
  
  def fibsByUnfold(): Stream[Int] = unfold((0,1)){case (f1, f2) => Some((f1, (f2, f1 + f2)))}
  
  def main(args: Array[String]): Unit = {
    println(fibsByUnfold().take(10).toList)
    
  }
}