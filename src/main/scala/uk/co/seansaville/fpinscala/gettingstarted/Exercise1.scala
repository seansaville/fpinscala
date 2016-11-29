package uk.co.seansaville.fpinscala.gettingstarted

import scala.annotation.tailrec

/**
  * Exercise 2.1: Write a recursive function to get the nth Fibonacci number.
  */
object Exercise1 {

  def fib(n: Int): Int = {
    @tailrec
    def fibAcc(n: Int, prev1: Int, prev2: Int): Int = {
      if (n == 0) prev1
      else fibAcc(n - 1, prev2, prev1 + prev2)
    }
    fibAcc(n, 0, 1)
  }

}
