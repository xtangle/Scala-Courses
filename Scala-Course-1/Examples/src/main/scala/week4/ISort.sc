object exercise {

  def insert(x: Int, xs: scala.List[Int]): scala.List[Int] = xs match {
    case scala.List() => scala.List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  def isort(xs: scala.List[Int]): scala.List[Int] = xs match {
    case scala.List() => scala.List()
    case y :: ys => insert(y, isort(ys))
  }

  isort(scala.List(7, 3, 9, 2))

  scala.Nil ::: scala.Nil
}