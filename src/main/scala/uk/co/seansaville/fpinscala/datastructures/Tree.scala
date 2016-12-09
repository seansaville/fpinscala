package uk.co.seansaville.fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25: Write a function that counts the number of nodes in a tree.
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
    * Exercise 3.26: Write a function that returns the maximum element in a tree of integers.
    */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n)      => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * Exercise 3.27: Write a function that returns the maximum path length from the root of a tree
    * to any of its leaves.
    */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  /**
    * Exercise 3.28: Implement map for trees.
    */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * Exercise 3.29: Implement fold for trees, then reimplement all of the previous functions in
    * terms of fold.
    */
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(n => n)(_ max _)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((ld, rd) => (ld max rd) + 1)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
