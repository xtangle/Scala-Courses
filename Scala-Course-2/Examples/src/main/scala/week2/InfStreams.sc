object exercise {

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  val nats: Stream[Int] = from(0)
  val m4s: Stream[Int] = nats map (_ * 4)

  (m4s take 100).toList

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes: Stream[Int] = sieve(from(2))

  (primes take 5).toList

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001

  sqrtStream(4).take(10).toList
}