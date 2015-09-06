import scala.annotation.tailrec

/**
 * Created by lkhamsurenl on 9/5/15.
 */
// Functor is a trait defining a type with one hole in it
trait Functor[F[_]] {
  def map[A, B](as: F[A])(f: A => B): F[B]
}

trait Applicitive[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](as: F[A])(f: A => B): F[B] =
    map2(as, unit(()))((a: A, _) => f(a)) // 2nd part is not relevant

  // traverse list while applying f
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((x,y) => map2(f(x),y)(_::_))

  // LIst[F] => F[List]
  def sequence[A, B](as: List[F[A]]): F[List[A]] =
    as.foldRight(unit(List[A]()))((fa: F[A], fl: F[List[A]]) => map2(fa, fl)(_::_))

  // replicate n times Monoid
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    @tailrec
    def recReplicateM[A](n: Int, fa: F[A], acc: F[List[A]]): F[List[A]] =
      if (n == 0) {
        unit(List[A]())
      } else {
        recReplicateM(n - 1, fa, map2(fa, acc)(_::_))
      }
    recReplicateM(n, fa, unit(List[A]()))
  }
}

trait Applicitive2[F[_]] extends Functor[F] {
  // primitives
  def unit[A](a: => A): F[A]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  // derived
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A => B](f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val ff: F[(A, B) => C] = unit(f)
    apply(ff)((fa, fb))
  }

}
