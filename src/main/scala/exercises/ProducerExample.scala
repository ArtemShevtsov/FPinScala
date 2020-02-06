package exercises
import scala.runtime.NonLocalReturnControl
import scala.util.control.ControlThrowable

object ProducerExample {
  def main(args: Array[String]): Unit = {
    functWithConsumer("Ex 1")(() => println("Producer 1"))
    functWithConsumer("Ex 1.1")(f = () => println("Producer 1.1"))
    functWithConsumer("Ex 1.2")(f = println("Producer 1.2"))
    functWithConsumer("Ex 2"){println("Producer 2")}
    functWithConsumer("Ex 3")(println("Producer 3"))

    def ff(): Unit = {
      println("Function ff")
    }

    functWithConsumer("Ex 4")(f = ff())

    def value: Int = {
      def one(x: Int): Int = { return x; 1 }
      val two = (x: Int) => { return 8; 2 }
      try {
        1 + one(2) + two(8)
      } catch  {
        case e: NonLocalReturnControl[Any] =>
          e.printStackTrace()
          throw new NonLocalReturnControl[Any]("asdf", "d")
      }
      return 15
    }

    println("1 + one(2) + two(8) = " + value)
  }

  def functWithConsumer(a: String)(f: => Unit): Unit = {
    println(a)
    f
  }
}
