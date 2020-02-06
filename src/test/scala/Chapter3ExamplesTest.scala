import book.examples.chapter3.{Cons, List, Nil}

object Chapter3ExamplesTest {
  private def x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  def main(args: Array[String]): Unit = {
    assert(x == 3)
    initTest()

//    println ("Length is: " + List.length(List(1,12,3)))
//    println("SUM using foldLeft 1,12,3: " + List.sumFoldLeft(List(1,12,3)))
//    println("PRODUCT using foldLeft 1,12,3: " + List.productFoldLeft(List(1,12,3)))
//
//    println("REVERSE of List(1,12,3, 4): " + List.reverse(List(1,12,3, 4)))
//    println("Append 5 to List(1,12,3, 4): " + List.appendFolding(List(1,12,3, 4), List(5)))
//
//    println("Flap Map of List(List(1,2), List(5, 6), List(3, 4)): " + List.flatMapping(List(List(1,2), List(5, 6), List(3, 4))))
//
//    val intList = List(1,2,3)
//    println("\n\nIntList before: " + intList)
//    println("Each plus one List(1,2,3): " + List.mapPlusOne(intList))
//    println("IntList after: " + intList)

//    println("\n\nEach doubleToString List(1.2556, 2.5782, 3.69715): " + List.mapDoubleToString(List(1.2556, 2.5782, 3.69715)))
//
//    println("\n\nIntList before: " + intList)
//    println("Map function square function(1,2,3): " + List.map(intList)(i => i*i))
//    println("IntList after: " + intList)
//
//    val longIntList = List(1,2,3,5,6,2,4,1,1,1,1,1,1)
//    println("\n\nIntList before: " + longIntList)
//    println("Filter List(1,2,3,5,6,2,4,1,1,1,1,1,1) with _ == 2 condition: " + List.filter(longIntList)(_ == 2))
//    println("Filter List(1,2,3,5,6,2,4,1,1,1,1,1,1) with _ > 3 condition: " + List.filter(longIntList)(_ > 3))
//    println("IntList after: " + longIntList)
//
//
//    val intList = List(1,2,3)
//    println("\n\nIntList before: " + intList)
//    println("Flat Map List(1,2,3) i => List(i,i*2): " + List.flatMap(intList)(i => List(i,i*2)))
//    println("IntList after: " + intList)

    println("Has Subsequence List(1,2,3,4,5) and List(3,4,5): " + List.hasSubsequence(List(1,2,3,4,5), List(3,4,5)))
    println("Has Subsequence List(1,2,3,4,5) and List(1,2,3): " + List.hasSubsequence(List(1,2,3,4,5), List(1,2,3)))
    println("Has Subsequence List(1,2,3,4,5) and List(1): " + List.hasSubsequence(List(1,2,3,4,5), List(1)))
    println("Has Subsequence List(1,2,3,4,5) and List(5): " + List.hasSubsequence(List(1,2,3,4,5), List(5)))
    println("Has Subsequence List(1,2,3,4,5) and List(1,4): " + List.hasSubsequence(List(1,2,3,4,5), List(1,4)))
    println("Has Subsequence List(1,2,3,4,5) and List(7): " + List.hasSubsequence(List(1,2,3,4,5), List(7)))
  }

  def initTest(): Unit = {
    val list = List(1,2,3,4,5)
    assert(List.init(list).equals(List(1,2,3,4)))
  }
}
