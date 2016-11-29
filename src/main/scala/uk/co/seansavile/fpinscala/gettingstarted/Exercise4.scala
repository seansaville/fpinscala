package uk.co.seansavile.fpinscala.gettingstarted

/**
  * Exercise 4: Implement uncurrying.
  */
object Exercise4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

}
