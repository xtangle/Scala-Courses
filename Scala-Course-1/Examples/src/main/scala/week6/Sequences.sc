object exercise {

  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val str = "Hello World!"
  str filter (c => c.isUpper)

  val r: Range = 1 until 5     // 1, 2, 3, 4
  val s: Range = 1 to 5        // 1, 2, 3, 4, 5
  val t: Range = 1 to 10 by 3  // 1, 4, 7, 10
  val u: Range = 6 to 1 by -2  // 6, 4, 2

  r mkString ", "
  s exists 4.equals
  s forall (_ <= 4)

  val pairs = str zip r
  pairs.unzip

  str flatMap (c => List('.', c))

  xs.max
  xs.min
  xs.sum

  (1 to 3) flatMap (x => (4 to 6) map (y => (x, y)))

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    ((xs zip ys) map (xy => xy._1 * xy._2)).sum

  scalarProduct(r.toVector.map(_.toDouble), s.toVector.map(_.toDouble))

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs zip ys).map { case (x, y) => x * y }.sum
  }

  scalarProduct2(r.toVector.map(_.toDouble), s.toVector.map(_.toDouble))

  def isPrime(n: Int): Boolean =
    if (n <= 1) false
    else (2 to math.sqrt(n).toInt) forall (n % _ != 0)

  List(1, 2, 3, 4, 5, 6, 7, 8).map(isPrime)

}