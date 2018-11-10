import common._

object ParallelReduce {

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => f(reduce[A](l, f), reduce[A](r, f))
  }

  def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) =>
      val (lV, rV) = parallel(reduce[A](l, f), reduce[A](r, f))
      f(lV, rV)
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Node(l, r) => Node(map[A, B](l, f), map[A, B](r, f))
  }

  def toList[A](t: Tree[A]): List[A] = t match {
    case Leaf(v) => List(v)
    case Node(l, r) => toList[A](l) ++ toList[A](r)
  }

  def toList2[A](t: Tree[A]): List[A] =
    reduce[List[A]](map[A, List[A]](t, List(_)), _ ++ _)

  def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
  def fMinus = (x: Int, y: Int) => x - y
  def res = reduce[Int](tree, fMinus)
  def resPar = reducePar[Int](tree, fMinus)

  res
  resPar

  map(tree, (x: Int) => 2 * x)
  toList(tree)
  toList2(tree)

  val threshold = 5

  def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
    if (right - left < threshold) {
      var res = inp(left); var i = left + 1
      while (i < right) {
        res = f(res, inp(i))
        i = i + 1
      }
      res
    } else {
      val mid = left + (right - left)/2
      val (a1, a2) = parallel(reduceSeg(inp, left, mid, f),
        reduceSeg(inp, mid, right, f))
      f(a1, a2)
    }
  }

  def reduceArr[A](inp: Array[A], f: (A, A) => A): A =
    reduceSeg(inp, 0, inp.length, f)

  reduceArr(Array(1, 3, 8), (x: Int, y: Int) => x + y)

}