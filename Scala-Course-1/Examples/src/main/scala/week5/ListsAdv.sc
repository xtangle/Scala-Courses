object exercise {

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => (y * y) :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: _ =>
      val (first, rest) = xs.span(x.equals)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(l => (l.head, l.length))

  val list = List(1, 2 ,3, 4)
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")
  val data = List("a", "a", "a", "b", "c", "c", "a")

  squareList1(list)
  squareList2(list)

  nums.filter(x => x > 0)
  nums.filterNot(x => x > 0)
  nums.partition(x => x > 0)

  nums.takeWhile(x => x > 0)
  nums.dropWhile(x => x > 0)
  nums.span(x => x > 0)

  pack(data)
  encode(data)
}