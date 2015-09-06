/**
 * Created by luvsandondov on 8/13/15.
 */
sealed trait Option[+A] {
  // map value of context to sth else
  def map[B](f: A => B): Option[B] = this match {
    case Some(w) => Some(f(w))
    case None => None
  }
  // map value to which produces context, then put it into context
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(w) => f(w)
    case None => None
  }
  // unwrap from the context, if None return default
  // B >: A means B is a supertype of A (meaning B = Vehicle, A = Car). default: => B means default is lazy
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(w) => w
    case None => default
  }
  // if None, then return ob: Option[B]
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(w) => Some(w)
    case None => ob
  }
  // filter the option to return Option
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(w) => if (f(w)) Some(w) else None
    case None => None
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _   => Some(xs.sum / xs.size)
  }
  // computes the variance of a values
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) match {
      case None => None
      case Some(m) => {
        val mapped = xs.map(v => math.pow(v - m, 2))
        mean(mapped)
      }
    }
  }
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  // recursive version
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => None
    case x::xs => x.flatMap(v => sequence(xs).map(v::_))
  }
  // using fold and map
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x,y)(_ :: _))
}


sealed trait Either[+E, +A] {
  // map right side to a new value
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }
  def leftMap[B](f: E => B): Either[B, A] = this match {
    case Right(v) => Right(v)
    case Left(e) => Left(f(e))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }
  def orElse[EE>: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(e) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}
case class Right[+A](value: A) extends Either[Nothing, A]
case class Left[+E](value: E) extends Either[E, Nothing]

object Either1 {
  def sequence1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(List[A]()) // empty
    case x::xs => x.map2(sequence1(xs))((x, y) => x::y)
  }
  // using folleft to compute
  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    es.foldRight[Either[E, List[A]]](Right(Nil))((x,y) => x.map2(y)(_::_))
  
  def traverse1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x::xs => f(x).map2(traverse1(xs)(f))((x: B, y: List[B]) => x::y)
  }
  
  def main(args: Array[String]): Unit = {
    val l = List[Either[String, Int]](Right(1), Left("e1"), Right(2), Left("e2"))
    println(sequence1(l))
    println(s"2nd implementation: ${sequence2(l)}")
    
    println(traverse1(List(1, 1, 2, 3))(x => if (x > 0) Right(x) else Left("negative")))
    
  }
}