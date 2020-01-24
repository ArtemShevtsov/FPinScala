import book.examples.chapter3.Cons
import book.examples.chapter3.List
import book.examples.chapter3.Nil
import book.exercises.Chapter2

object Chapter2ExamplesTest{
  def main(args: Array[String]): Unit = {
    println(List.init(List(1,2,3,4)))
    println (List.init(List(4)))
    println (List.init(Nil))

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    print (x)

    testFibonachi()
    testIsSorted()
  }

  def testFibonachi(): Unit = {
    assert(Chapter2.fib(0) == 0)
    assert(Chapter2.fib(1) == 1)
    assert(Chapter2.fib(2) == 1)
    assert(Chapter2.fib(3) == 2)
    assert(Chapter2.fib(4) == 3)
    assert(Chapter2.fib(5) == 5)
    assert(Chapter2.fib(6) == 8)
    assert(Chapter2.fib(7) == 13)
    assert(Chapter2.fib(8) == 21)
    assert(Chapter2.fib(9) == 34)
  }

  def testIsSorted(): Unit = {
    val some1 = Some(1)
    val some2 = Some(2)
    val some3 = Some(3)
    val some4 = Some(4)
    val some5 = Some(5)

    def compareAsc(s1: Some, s2: Some): Boolean = s1.size <= s2.size
    def compareDesc(s1: Some, s2: Some): Boolean = s1.size >= s2.size

    val singleElementArray: Array[Some] = Array(some1)
    val twoSameElementArray: Array[Some] = Array(some1, some1)
    val notSortedArray: Array[Some] = Array(some5, some2, some4, some1, some3)
    val isAscSortedArray: Array[Some] = Array(some1, some2, some3, some4, some5)
    val isDescSortedArray: Array[Some] = Array(some5, some4, some3, some2, some1)

    assert(Chapter2.isSorted(singleElementArray, compareAsc))
    assert(Chapter2.isSorted(singleElementArray, compareDesc))

    assert(!Chapter2.isSorted(notSortedArray, compareAsc))
    assert(!Chapter2.isSorted(notSortedArray, compareDesc))

    assert(Chapter2.isSorted(isAscSortedArray, compareAsc))
    assert(Chapter2.isSorted(isDescSortedArray, compareDesc))

    assert(!Chapter2.isSorted(isDescSortedArray, compareAsc))
    assert(!Chapter2.isSorted(isAscSortedArray, compareDesc))

    assert(Chapter2.isSorted(twoSameElementArray, compareAsc))
    assert(Chapter2.isSorted(twoSameElementArray, compareDesc))
  }
}


private case class Some(size: Int) {
}