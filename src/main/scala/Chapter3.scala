/**
 * Created by luvsandondov on 8/10/15.
 */

object Drop {
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case x::xs => xs
  }
  // drop first n elements
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
    case x::xs => {
      if (predicate(x) == true) dropWhile(xs, predicate)
      else as
    }
  }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => List(a)
    case x::xs => a::xs // return a new list first element replaced
  }
}

object Fold {
  // folds List[A] to produce B
  def foldRight[A, B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil   => z
    case x::xs => f(x, foldRight(xs, z)(f))
  }

  /*
  * implementation of foldLeft
  * * * */
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x::xs => foldLeft(xs, f(x, z))(f)
  }
  //
  def foldLeft2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldRight(reverse(as), z)(f)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)
  def prod2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldLeft2(as, 0)((x, y) => y + 1)

  // reverse a list with fold
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((x, l) => x::l)
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
  
  // map then foldLeft to produce the myFlatMap
  def myFlatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    // map
    val mapped = myMap(as)(f)
    Fold.foldLeft(mapped, List[B]())((l, lOfl) => lOfl ++ l)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  // size of the tree
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 1
    case Branch(v, l: A, r: A) => 1 + size(r) + size(l)
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
  
}