import scala.annotation.tailrec

/**
 * Created by luvsandondov on 8/10/15.
 */

object Drop {
  // Get list without the first element:
  def tail[A](as: List[A]): List[A] = as match {
    case Nil   => as
    case x::xs => xs
  }

  // Drop the first n elements of a given list:
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case 0 => as
    case x => drop(tail(as), x - 1)
  }

  /*
  * drop while the predicate is true
  * @param as: list 
  * @param predicate: predicate to check if satisfies
  * */
  def dropWhile[A](as: List[A], predicate: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x::xs => if (predicate(x)) dropWhile(xs, predicate) else as
  }
  // This version of dropWhile can be called with anonymous function with like:
  // dropWhile1(List(1,2,3))(x => x > 2)
  def dropWhile1[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x::xs => if (f(x)) dropWhile1(xs)(f) else as
  }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => List(a)
    case x::xs => a::xs // return a new list first element replaced
  }
}

object Fold {
  // First hit the end of the list before recursing back.
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x::xs => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x::xs => foldLeft(xs, f(z,x))(f)
  }

  // Length of the list using the foldRight.
  def length[A](as: List[A]): Int =
    foldLeft(as, 0)((b: Int, a: A) => 1 + b)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((z: List[A], a: A) => a::z)

  // Implement foldRight in terms of foldLeft.
  def foldRight1[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(b, a)))(z)

  def concat[A](as: List[A], bs: List[A]): List[A] = as match {
    case List() => bs
    case x::xs  => x::concat(xs, bs)
  }

  def mergeLists[A](ass: List[List[A]]): List[A] =
    foldLeft(ass, List[A]())((x,y) => concat(x,y))
}

object Map {
  def myMap[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case x::xs => f(x)::myMap(xs)(f)
  }
  
  def myFilter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x::xs => if (f(x)) x::myFilter(xs)(f) else myFilter(xs)(f)
  }
  
  def myFlatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    Fold.foldLeft(as, List[B]())((bs, a) => Fold.concat(bs, f(a)))

  def myFilterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    myFlatMap(as)((a: A) => if (f(a)) List(a) else List())

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (List(), List()) => List()
    case (x::xs, y::ys)   => f(x,y)::zipWith(xs, ys)(f)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  // size of the tree
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(v: A) => 1
    case Branch(v: A, l: A, r: A) => 1 + size(r) + size(l)
  }

  // maximum element of a tree
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(v, l, r) => Math.max(v, Math.max(maximum(l), maximum(r)))
  }
  // depth of a tree
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 0
    case Branch(v, l, r) => 1 + Math.max(depth(l), depth(r))
  }
  // maps each element of a tree
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(v, l, r) => Branch(f(v), map(l, f), map(r, f))
  }

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
    case Leaf(v)         => f(v, z)
    case Branch(v, l, r) => f(v, fold(l, fold(r, z)(f))(f))
  }
  
}