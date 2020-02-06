package exercises

import scala.annotation.tailrec

object Chapter2{
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, first: Int, second: Int): Int =
      if(n <= 0) first
      else go(n - 1, second, first + second)

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int): Boolean =
      if(n + 1 >= as.length)
        true
      else if(!ordered(as(n), as(n + 1)))
        false
      else loop(n + 1)
    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A =>
      (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) =>
      f(a)( b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}
