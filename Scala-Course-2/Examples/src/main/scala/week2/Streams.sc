object exercise {

  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }

  streamRange(1, 10).take(3).toList

  (1 to 10).toStream

  "Hello world".toStream take 5 toList
}