package book.examples.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x*product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  def setHead[A](l: List[A], el: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Cant setHead on Nil!!!")
    case Cons(_,xs) => Cons(el, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x,xs) => Cons(x, append(xs, a2))
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0)
       l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if(f(h)) dropWhile(t, f)
      else Cons(h, t)

//    case Cons(h, t) if(h) => dropWhile(t, f)
//    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Cant init on Nil!!!")
    case Cons(_, Nil) => Nil
    case Cons(h, t) =>
      def go(ll: List[A]): List[A] = {
        ll match {
          case Cons(_, Nil) => Nil
          case Cons(h, t) => Cons(h, go(t))
        }
      }

      Cons(h, go(t))
  }

  //function with initial Z value
  // will be applied form the most right element first, and then moving to the left
//  @scala.annotation.tailrec
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //function with initial Z value to the most left element
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((a, b) => f(b, a))
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def appendFolding[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_,_))
  }

  def reverse[A](as: List[A]) :List[A] = {
    foldLeft(as, Nil: List[A])((b, la) => Cons(la, b))
  }

  def sum2(ints: List[Int]): Int = {
    foldRight(ints, 0)(_ + _) // same for foldRight(ints, 0)((x, y) => x + y)
  }

  def product2(ds: List[Double]): Double = {
    foldRight(ds, 1d)(_ * _) // same for foldRight(ints, 0)((x, y) => x * y)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, b) => b + 1)
  }

  def sumFoldLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def productFoldLeft(ints: List[Int]): Double = {
    foldLeft(ints, 1d)(_ * _)
  }

  def flatMapping[A](ll: List[List[A]]): List[A] = {
    foldLeft(ll, Nil: List[A])(append)
  }

  def mapPlusOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  def mapDoubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
//    foldRight(as, Nil: List[A])((a, b) => {
//      if(f(a)){
//        Cons(a, b)
//      } else {
//        b
//      }
//    })

    flatMap(as)(a => {
      if(f(a)) List(a)
      else Nil
    })
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil: List[B])((b, a) => append(b, f(a)))
//    foldLeft(map(as)(f), Nil: List[B])(append)
  }

  def pairedSum(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, b), Cons(x, y)) => Cons(a + x, pairedSum(b, y))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, b), Cons(x, y)) => Cons(f(a, x), zipWith(b, y)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(a, b), Cons(x, Nil)) =>
      if(a == x) true
      else hasSubsequence(b, sub)
    case (Cons(a, b), Cons(x, y)) =>
      if(a == x) startsWith(b, y)
      else hasSubsequence(b, sub)
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (_, Nil) => false
    case (Nil, _) => false
    case (Cons(a, b), Cons(x, Nil)) => a == x
    case (Cons(a, b), Cons(x, y)) => a == x && startsWith(b, y)
  }
}
