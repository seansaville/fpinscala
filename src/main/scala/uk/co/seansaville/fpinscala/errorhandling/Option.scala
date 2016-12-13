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

  /**
    * Exercise 4.3: Implement map2, which combines two Option values using a binary function.
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a2 => b map (b2 => f(a2, b2)))

  /**
    * Exercise 4.4: Implement sequence, which combines a list of Options into one option.
    */
  def sequence[A](ls: List[Option[A]]): Option[List[A]] = ls match {
    case Nil     => Some(Nil)
    case a :: as => a flatMap (a2 => sequence(as) map (a2 :: _))
  }

  /**
    * Exercise 4.5: Implement traverse.
    */
  def traverse[A, B](ls: List[A])(f: A => Option[B]): Option[List[B]] = ls match {
    case Nil     => Some(Nil)
    case a :: as => map2(f(a), traverse(as)(f))(_ :: _)
  }

  /**
    * And then sequence in terms of traverse.
    */
  def sequenceByTraverse[A](ls: List[Option[A]]): Option[List[A]] = traverse(ls)(e => e)

}