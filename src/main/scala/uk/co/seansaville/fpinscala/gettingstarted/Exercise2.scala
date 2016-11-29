package uk.co.seansaville.fpinscala.gettingstarted

import scala.annotation.tailrec

/**
  * Exercise 2.2: Implement isSorted, which checks whether an Array[A] is sorted according to a
  * given comparison function.
  */
object Exercise2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }
    loop(0)
  }

}
