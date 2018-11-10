object exercise {

  def isPrime(n: Int): Boolean =
    if (n <= 1) false
    else (2 to math.sqrt(n).toInt) forall (n % _ != 0)

  val n: Int = 7

  (2 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (p =>
      isPrime(p._1 + p._2))

  for {
    i <- 2 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum

  scalarProduct(Vector(1.0, 2.0, 3.0), Vector(1.0, 2.0, 3.0))
}