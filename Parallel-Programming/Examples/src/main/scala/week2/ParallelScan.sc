import common._

object ParallelScan {

  val threshold = 5

  /**
    * Parallel scanLeft using reduce and map
    **/
  def scanLeftSeq[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    var i = 0
    out(0) = a0
    while (i < inp.length) {
      out(i + 1) = f(out(i), inp(i))
      i = i + 1
    }
  }

  def scanLeft1[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    val fi = (i: Int, v: A) => reduceSeg1(inp, 0, i, a0, f)
    mapSeg(inp, 0, inp.length, fi, out)
    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last))
  }

  val a: Array[Int] = Array(1, 3, 8, 12, 20, 31)
  val b1: Array[Int] = Array.fill(a.length + 1)(0)
  val b2: Array[Int] = Array.fill(a.length + 1)(0)
  val a0: Int = 100
  val f: (Int, Int) => Int = (x: Int, y: Int) => x + y

  scanLeftSeq(a, a0, f, b1)
  scanLeft1(a, a0, f, b2)
  b1
  b2

  /**
    * Parallel tree scanLeft using intermediate tree
    **/
  def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) =>
      val (lt, rt) = parallel(upsweep(l, f), upsweep(r, f))
      NodeRes(lt, f(lt.res, rt.res), rt)
  }

  def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
    case LeafRes(v) => Leaf(f(a0, v))
    case NodeRes(l, _, r) =>
      val (lt, rt) = parallel(downsweep[A](l, a0, f),
        downsweep[A](r, f(a0, l.res), f))
      Node(lt, rt)
  }

  def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
    case leaf @ Leaf(_) => Node(Leaf(x), leaf)
    case Node(l, r) => Node(prepend(x, l), r)
  }

  def scanLeft2[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val scan1 = downsweep(tRes, a0, f)
    prepend(a0, scan1)
  }

  val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
  val plus: (Int, Int) => Int = (x: Int, y: Int) => x + y
  val r1: TreeRes[Int] = upsweep(t1, plus)
  downsweep(r1, 100, plus)
  scanLeft2(t1, 100, plus)

  /**
    * Parallel array scanLeft using intermediate tree
    **/
  def upsweepA[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
    if (to - from < threshold)
      LeafResA(from, to, reduceSeg1(inp, from + 1, to, inp(from), f))
    else {
      val mid = from + (to - from) / 2
      val (lt, rt) = parallel(upsweepA(inp, from, mid, f), upsweepA(inp, mid, to, f))
      NodeResA(lt, f(lt.res, rt.res), rt)
    }
  }

  def downsweepA[A](inp: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A],
                    out: Array[A]): Unit = t match {
    case LeafResA(from, to, _) =>
      scanLeftSeg(inp, from, to, a0, f, out)
    case NodeResA(l, _, r) =>
      parallel(downsweepA(inp, a0, f, l, out),
        downsweepA(inp, f(a0, l.res), f, r, out))
  }

  def scanLeftSeg[A](inp: Array[A], left: Int, right: Int,
                     a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    if (left < right) {
      var i = left
      var a = a0
      while (i < right) {
        a = f(a, inp(i))
        i = i + 1
        out(i) = a
      }
    }
  }

  def scanLeft3[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    val t = upsweepA(inp, 0, inp.length, f)
    downsweepA(inp, a0, f, t, out) // fills out[1..inp.length]
    out(0) = a0 // prepends a0
  }

  val a2 = Array(1, 3, 8, 50)
  val b3: Array[Int] = Array.fill(a2.length + 1)(0)
  scanLeft3(a2, 100, plus, b3)
  b3

  /**
    * reduceSeg
    **/
  def reduceSeg1Seq[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
    var res = inp(left)
    var i = left + 1
    while (i < right) {
      res = f(res, inp(i))
      i = i + 1
    }
    res
  }
  def reduceSeg1Par[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
    if (right - left < threshold) {
      reduceSeg1Seq(inp, left, right, f)
    } else {
      val mid = left + (right - left) / 2
      val (a1, a2) = parallel(reduceSeg1Par(inp, left, mid, f),
        reduceSeg1Par(inp, mid, right, f))
      f(a1, a2)
    }
  }
  def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {
    if (right > left) {
      f(a0, reduceSeg1Par(inp, left, right, f))
    } else {
      a0
    }
  }

  /**
    * mapSeg
    **/
  def mapSegSeq[A, B](inp: Array[A], left: Int, right: Int, fi: (Int, A) => B,
                      out: Array[B]): Unit = {
    var i = left
    while (i < right) {
      out(i) = fi(i, inp(i))
      i = i + 1
    }
  }
  def mapSeg[A, B](inp: Array[A], left: Int, right: Int, fi: (Int, A) => B,
                      out: Array[B]): Unit = {
    if (right - left < threshold)
      mapSegSeq(inp, left, right, fi, out)
    else {
      val mid = left + (right - left) / 2
      parallel(mapSeg(inp, left, mid, fi, out),
        mapSeg(inp, mid, right, fi, out))
    }
  }

  /**
    * tree storing input collection
    **/
  sealed abstract class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  /**
    * tree storing intermediate values
    **/
  sealed abstract class TreeRes[A] { val res: A }
  case class LeafRes[A](override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

  /**
    * intermediate tree for array reduce
    **/
  sealed abstract class TreeResA[A] { val res: A }
  case class LeafResA[A](from: Int, to: Int, override val res: A) extends TreeResA[A]
  case class NodeResA[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]
}