package uk.co.seansaville.fpinscala.errorhandling

sealed trait Option[+A] {

  /**
    * Exercise 4.1: Implement map, flatMap, getOrElse, orElse, and filter.
    */
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(b) => b
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None    => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _               => None
  }

}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

object Option {

  /**
    * Exercise 4.2: Implement the variance function in terms of flatMap.
    */
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}