object exercise {

  val list1: List[Int] = List(1, 2 ,3, 4)
  val list2: List[Int] = List(2, -4, 5, 7, 1)

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( f(_) :: _ )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (_, l) => l + 1 )

  concat(list1, list2)
  mapFun(list1, (x: Int) => x * 2.5)
  lengthFun(list2)

}