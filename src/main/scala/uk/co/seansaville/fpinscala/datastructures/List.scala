package uk.co.seansaville.fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

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

    //TODO: reverse this once I've written the reverse method!
    iterate(ls, Nil)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
    * Exercise 3.9: Compute the length of a list using foldRight.
    */
  def length[A](ls: List[A]): Int = {
    foldRight(ls, 0)((_, n) => n + 1)
  }

}
