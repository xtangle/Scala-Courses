import common._

object ParallelMap {

  def mapSeq[A, B](lst: List[A], f: A => B): List[B] = lst match {
    case Nil => Nil
    case h :: t => f(h) :: mapSeq(t, f)
  }

  def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B,
                       out: Array[B]) = {
    // Writes to out(i) for left <= i <= right - 1
    var i = left
    while (i < right) {
      out(i) = f(inp(i))
      i = i + 1
    }
  }

  val in = Array(2, 3, 4, 5, 6)
  val out = Array.fill(in.length)(0)
  val f = (x: Int) => x * x

  mapASegSeq(in, 1, 3, f, out)
  out

  val threshold = 5

  def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B,
                       out: Array[B]): Unit = {
    // Writes to out(i) for left <= i <= right - 1
    if (right - left < threshold)
      mapASegSeq(inp, left, right, f, out)
    else {
      val mid = left + (right - left)/2
      parallel(mapASegPar(inp, left, mid, f, out),
        mapASegPar(inp, mid, right, f, out))
    }
  }

  def power(x: Int, p: Double): Int =
    math.exp(p * math.log(math.abs(x))).toInt

  val inpA: Array[Int] = Array(1, 2, 3, 4, 5)
  val outA: Array[Double] = Array.fill(inpA.length)(0.0)
  val outB: Array[Double] = Array.fill(inpA.length)(0.0)
  val p: Double = 1.5
  def g: (Int) => Double = (x: Int) => power(x, p)

  mapASegSeq(inpA, 0, inpA.length, g, outA)
  mapASegPar(inpA, 0, inpA.length, g, outB)

  outA
  outB

  def normsOfSeq(inp: Array[Int], p: Double,
              left: Int, right: Int,
              out: Array[Double]): Unit = {
    var i = left
    while (i < right) {
      out(i) = power(inp(i), p)
      i = i + 1
    }
  }

  def normsOfPar(inp: Array[Int], p: Double,
                 left: Int, right: Int,
                 out: Array[Double]): Unit = {
    if (right - left < threshold)
      normsOfSeq(inp, p, left, right, out)
    else {
      val mid = left + (right - left)/2
      parallel(normsOfPar(inp, p, left, mid, out),
        normsOfPar(inp, p, mid, right, out))
    }
  }

  sealed abstract class Tree[A] {
    val size: Int
  }
  case class Leaf[A](a: Array[A]) extends Tree[A] {
    override val size: Int = a.length
    override def toString: String = a.mkString("[", ",", "]")
  }
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    override val size: Int = l.size + r.size
    override def toString: String = s"($l,$r)"
  }

  def mapTreePar[A: Manifest, B: Manifest](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(a) =>
      val len = a.length
      val b = new Array[B](len)
      var i = 0
      while (i < len) {
        b(i) = f(a(i))
        i = i + 1
      }
      Leaf(b)
    case Node(l, r) =>
      val (lb, rb) = parallel(mapTreePar(l, f), mapTreePar(r, f))
      Node(lb, rb)
  }

  val t = Node(Leaf(Array(1)), Node(Leaf(Array(2, 3)), Leaf(Array(4, 5))))
  mapTreePar(t, g)
}