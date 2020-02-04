package book.exercises
import scala.language.postfixOps

object SortArray {
  def sort(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        sort(xs filter(a => a < pivot)),
        xs filter (pivot ==),
        sort(xs filter (pivot <)))
    }

  }

  def main(str: Array[String]): Unit = {
    val a: Array[Int] = Array(1,8,9,5,7,3,2)
    sort(a).foreach(println)
  }
}
