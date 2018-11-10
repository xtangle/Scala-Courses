import week4._

object exercise {

  List()

  List(1)

  List(1, 2)

  List(1, 2, 3)

  List(4, 5, 6)

  val x: List[String] = List("Hello", "World")

  x.prepend("morning").prepend("Good")

  def f(xs: List[NonEmpty], x: Empty) = xs prepend x

}
