package uk.co.seansaville.fpinscala.datastructures

import com.sun.org.apache.xpath.internal.functions.FuncFalse
import sun.font.TrueTypeFont

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.2: Implement the function tail for removing the first element of a list.
    */
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil        => throw new NoSuchElementException
    case Cons(_, t) => t
  }

  /**
    * Exercise 3.3: Implement the function setHead for replacing the first element of a list.
    */
  def setHead[A](ls: List[A], a: A): List[A] = ls match {
    case Nil        => Cons(a, Nil)
    case Cons(_, t) => Cons(a, t)
  }

  /**
    * Exercise 3.4: Implement the function drop for removing the first n elements from a list.
    */
  def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case Nil        => Nil
    case Cons(_, t) => if (n <= 0) t else drop(t, n - 1)
  }

  /**
    * Exercise 3.5: Implement the function dropWhile for removing elements from a list while they
    * satisfy a predicate.
    */
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Nil        => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else ls
  }

  /**
    * Exercise 3.6: Implement the function init, which returns a list containing all but the last
    * element of a list.
    */
  def init[A](ls: List[A]): List[A] = {
    @tailrec
    def iterate(ls: List[A], res: List[A]): List[A] = ls match {
      case Nil          => throw new NoSuchElementException
      case Cons(_, Nil) => res
      case Cons(h, t)   => iterate(t, Cons(h, res))
    }

    reverse(iterate(ls, Nil))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
    * Exercise 3.9: Compute the length of a list using foldRight.
    */
  def length[A](ls: List[A]): Int = {
    foldRight(ls, 0)((_, n) => n + 1)
  }

  /**
    * Exercise 3.10: Implement foldLeft.
    */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    * Exercise 3.11: Implement sum, product, and a function to compute the length of a list using
    * foldLeft.
    */
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ints: List[Int]): Int = foldLeft(ints, 1)(_ * _)

  def length2[A](list: List[A]): Int = foldLeft(list, 0)((n, _) => n + 1)

  /**
    * Exercise 3.12: Write a function that returns the reverse of a list.
    */
  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, Nil.asInstanceOf[List[A]])((xs, x) => Cons(x, xs))

  /**
    * Exercise 3.13: Implement foldRight in terms of foldLeft.
    */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  /**
    * Exercise 3.14: Implement append in terms of either foldLeft or foldRight.
    */
  def append[A](ls1: List[A], ls2: List[A]): List[A] = foldRight2(ls1, ls2)(Cons(_, _))

  /**
    * Exercise 3.15: Write a function that concatenates a list of lists into a single list.
    */
  def concatenate[A](ls: List[List[A]]): List[A] = foldRight2(ls, Nil.asInstanceOf[List[A]])(append)

  /**
    * Exercise 3.16: Write a function that transforms a list of integers by adding 1 to each element.
    */
  def addOne(ints: List[Int]): List[Int] = foldRight2(ints, Nil: List[Int])((h, t) => Cons(h + 1, t))

  /**
    * Exercise 3.17: Write a function that turns each value in a list of doubles to a string.
    */
  def doubleToString(doubles: List[Double]): List[String] =
    foldRight2(doubles, Nil: List[String])((h, t) => Cons(h.toString, t))

  /**
    * Exercise 3.18: Implement map.
    */
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil: List[B])((h, t) => Cons(f(h), t))

  /**
    * Exercise 3.19: Implement filter.
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  /**
    * Exercise 3.20: Implement flatMap.
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))

  /**
    * Exercise 3.21: Use flatMap to implement filter.
    */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  /**
    * Exercise 3.22: Write a function that zips two lists by adding corresponding elements.
    */
  def zipSum(ls1: List[Int], ls2: List[Int]): List[Int] = {
    @tailrec
    def go(l: List[Int], r: List[Int], res: List[Int]): List[Int] = (l, r) match {
      case (Nil, _)                     => res
      case (_, Nil)                     => res
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(h1 + h2, res))
    }

    reverse(go(ls1, ls2, Nil))
  }

  /**
    * Exercise 3.23: Implement zipWith.
    */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def go(l: List[A], r: List[B], res: List[C]): List[C] = (l, r) match {
      case (Nil, _)                     => res
      case (_, Nil)                     => res
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(f(h1, h2), res))
    }

    reverse(go(as, bs, Nil))
  }

  /**
    * Exercise 3.24: Implement hasSubsequence.
    */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    /**
      * Checks if a list starts with some subsequence.
      */
    @tailrec
    def check(ls: List[A], seq: List[A]): Boolean = (ls, seq) match {
      case (_, Nil)                       => true
      case (Nil, _)                       => false
      case (Cons(h, t), Cons(subH, subT)) => if (h == subH) check(t, subT) else false
    }

    if (check(sup, sub))
      true
    else
      sup match {
        case Nil        => sub == Nil
        case Cons(_, t) => hasSubsequence(t, sub)
      }
  }

}
