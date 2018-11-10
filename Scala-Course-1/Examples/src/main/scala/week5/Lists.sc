object exercise {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(_) => List()
    case y :: ys => y :: init(ys)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => if (n == 0) ys else y :: removeAt(n - 1, ys)
  }

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case y :: ys => y match {
      case z: List[Any] => flatten(z) ::: flatten(ys)
      case _ => y :: flatten(ys)
    }
  }

  init(List(1, 2, 3, 4, 5))

  removeAt(1, List('a', 'b', 'c', 'd'))

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}