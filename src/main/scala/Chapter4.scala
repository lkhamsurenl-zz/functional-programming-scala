/**
 * Created by luvsandondov on 8/13/15.
 */
sealed trait Option[+A] {
  // map value of context to sth else
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  // map value to which produces context, then put it into context
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(a) => f(a)
  }

  // unwrap from the context, if None return default
  // B >: A means B is a supertype of A (meaning B = Vehicle, A = Car). default: => B means default is lazy
  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  // if None, then return ob: Option[B]
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => this
  }

  // filter the option to return Option
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case List() | Nil => None
    case _            => Some(xs.sum / xs.size)
  }

  // computes the variance of a values
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).map(me => xs.map(x => math.pow(x - me, 2)).sum)
  }
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      ao <- a
      bo <- b
    } yield f(ao, bo)

  // recursive version
  def sequenceRec[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil | List() => Some[List[A]](List[A]())
    case x::xs => map2(x, sequenceRec(xs))(_::_)
  }

  // Given a sequence of list of options, transform to option of list.
  def sequence[A](os: List[Option[A]]): Option[List[A]] =
    os.foldLeft(Some(List[A]()): Option[List[A]])((z: Option[List[A]], o: Option[A]) => map2(o, z)(_::_))

  // Given a list of elements, apply function to each element and return
  // 1. Option[List] of values if all succeed.
  // 2. None if any of them fails.
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil | List() => Some(List())
    case x::xs => map2(f(x), traverse(xs)(f))(_::_)
  }

  def sequenceUsingTraverse[A](os: List[Option[A]]): Option[List[A]] =
    traverse(os)(o => o)
}


sealed trait Either[+E, +A] {
  // map right side to a new value
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def leftMap[B](f: E => B): Either[B, A] = this match {
    case Left(e) => Left(f(e))
    case Right(a) => Right(a)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE>: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
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
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("Empty sequence mean!")
    else
      Right(xs.sum / xs.length)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) } // Return the exception on the left side

  // Walks through the list of eithers, return either[E, List]
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

  def traverse[E,A,B](es: List[Either[E, A]])(f: A => B): Either[E, List[B]] = es match {
    case Nil | List() => Right(List())
    case x::xs => x.map(f).map2(traverse(xs)(f))(_::_)
  }
}

case class Person(name: Name, age: Age)
object Person {
  def mkPerson(name: String, age: Int): Either[String, Person] =
    Name.mkName(name).map2(Age.mkAge(age))(Person(_, _))
}

sealed case class Name(val value: String)
object Name {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("No name given")
    else Right(Name(name))
}

sealed case class Age(val value: Int)
object Age {
  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is not positive")
    else Right(Age(age))
}