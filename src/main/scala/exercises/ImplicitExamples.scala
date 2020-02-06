package exercises

object ImplicitExamples extends App{
  /**
   * lengthCompare method not in String class but in {@see scala.collection.StringOps}
   *
   * method can be applied because of implicit method {@link scala.Predef::augmentString}
   * */
  implicit val str: String = "Nae"
  println(str.lengthCompare(3))

  val cs: String = "Hey, I am Implicit"
  implicit def makeImplicitCustomString(s: String): CustomString = CustomString(s)
  cs.customPrint()
  println(cs.take(8))

  cs.repeatMyNameFewTimes(5)


  def getIt[A](implicit a: A): A = a
  println("My real name is: " + getIt[String])
}


case class CustomString(s: String){
  def customPrint():Unit = {
    println("CUSTOM STRING IS: " + s)
  }

  def repeatMyNameFewTimes(n: Int)(implicit name: String):Unit = {
    for(_ <- 1 to n) {
      println("My name is ...: " + name)
    }
  }
}
