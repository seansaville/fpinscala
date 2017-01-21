package uk.co.seansaville.fpinscala.laziness

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  /**
    * Exercise 5.1: Write a function to convert a Stream to a List.
    */
  def toList: List[A] = this match {
    case Empty      => List()
    case Cons(h, t) => h() :: t().toList
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
