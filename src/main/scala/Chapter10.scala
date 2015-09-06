/**
 * Created by luvsandondov on 8/18/15.
 */
trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}

object Monoid1 {
  // shared trait  
  implicit val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }
  implicit val intAddMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }
  implicit val intMulMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 1
  }
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }
  // option monoid to combine option values
  def optionMonoid[A]: Monoid[Option[A]] = ???
  // map over list to create a monoid type, then fold [right, left]
  def foldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 =>  m.zero
    case 1 => f(v.head)
    case n =>  {
      // divide it in the half
      val (l, r) = v.splitAt(n/2)
      m.op(foldMap(l, m)(f), foldMap(r, m)(f))
    }
    
  }


  
    
  def main(args: Array[String]): Unit = {
    println(s"everything is good")
    print(foldMap(IndexedSeq("1","2","3"), intAddMonoid)((s:String) => s.toInt))
  }
}


trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = 
    foldLeft(as)(m.zero)(m.op)
  
}

