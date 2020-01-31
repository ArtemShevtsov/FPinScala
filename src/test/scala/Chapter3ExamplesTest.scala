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

    println ("Length is: " + List.length(List(1,12,3)))
    println("SUM using foldLeft 1,12,3: " + List.sumFoldLeft(List(1,12,3)))
    println("PRODUCT using foldLeft 1,12,3: " + List.productFoldLeft(List(1,12,3)))

    println("REVERSE of List(1,12,3, 4): " + List.reverse(List(1,12,3, 4)))
    println("Append 5 to List(1,12,3, 4): " + List.appendFolding(List(1,12,3, 4), List(5)))

  }

  def initTest(): Unit = {
    val list = List(1,2,3,4,5)
    assert(List.init(list).equals(List(1,2,3,4)))
  }
}
