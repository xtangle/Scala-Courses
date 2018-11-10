object exercise {

  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }

  def isPrime(n: Int): Boolean =
    if (n <= 1) false
    else (2 to math.sqrt(n).toInt) forall (n % _ != 0)


  def expr = {
    val x = { print("x"); 1 }
    lazy val y = { print("y"); 2 }
    def z = { print("z"); 3 }
    z + y + x + z + y + x
  }

  expr

  (streamRange(1000, 10000) filter isPrime) apply 1
}