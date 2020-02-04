package book.exercises

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
  }

  def functWithConsumer(a: String)(f: => Unit): Unit = {
    println(a)
    f
  }
}
