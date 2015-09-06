/**
 * Created by luvsandondov on 8/30/15.
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  // distribute the functor over the tuple
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = 
    (map(fab)(_._1), map(fab)(_._2))
  // takes e, of same types then distribute
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] extends Functor[F] {
  // Monad 
  // produces the unit value of the functor
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(F: A => F[B]): F[B]
  
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)((x, y) => x :: y))
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = 
    sequence(la.map(f))
  // replicate monad n times
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = n match {
    case 0 => unit(List[A]())
    case x => map2(ma, replicateM(n - 1, ma))(_ :: _)
  }
  
  def product[A, B](ma: F[A], mb: F[B]): F[(A ,B)] = 
    map2(ma, mb)((_, _))
  
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = 
    (a: A) => flatMap(f(a))(g)
  
  def join[A](mma: F[F[A]]): F[A] = ???
}

object Monad {
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = 
      fa.flatMap(f)
  }
  
  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List[A]()
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = 
      fa.flatMap(f)
  }
  
}