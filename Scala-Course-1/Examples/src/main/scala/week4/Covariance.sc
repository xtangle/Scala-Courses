import week4._

object exercise {
  val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
  // val b: Array[IntSet] = a // Arrays are not covariant in scala!
  val b: Array[IntSet] = Array(new NonEmpty(1, Empty, Empty))
  b(0) = Empty
  val s: NonEmpty = a(0)
}