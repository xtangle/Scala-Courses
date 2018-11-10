import common._

object SumSegment {

  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    var i = s; var sum: Int = 0
    while (i < t) {
      sum = sum + power(a(i), p)
      i = i + 1
    }
    sum
  }

  def power(x: Int, p: Double): Int = math.exp(p * math.log(math.abs(x))).toInt

  def pNorm(a: Array[Int], p: Double): Int =
    power(sumSegment(a, p, 0, a.length), 1/p)

  def pNormTwoPart(a: Array[Int], p: Double): Int = {
    val m = a.length / 2
    val (sum1, sum2) = parallel(
      sumSegment(a, p, 0, m),
      sumSegment(a, p, m, a.length)
    )
    power(sum1 + sum2, 1/p)
  }

  def pNormFourPart(a: Array[Int], p: Double): Int = {
    val m1 = a.length/4; val m2 = a.length/2; val m3 = 3*a.length/4
    val ((sum1, sum2), (sum3, sum4)) = parallel(
      parallel(sumSegment(a, p, 0, m1), sumSegment(a, p, m1, m2)),
      parallel(sumSegment(a, p, m2, m3), sumSegment(a, p, m3, a.length))
    )
    power(sum1 + sum2 + sum3 + sum4, 1/p)
  }

  def pNormRec(a: Array[Int], p: Double): Int =
    power(segmentRec(a, p, 0, a.length), 1/p)


  // like sumSegment but parallel
  val threshold = 4
  def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    if (t - s < threshold)
      sumSegment(a, p, s, t) // small segment: do it sequentially
    else {
      val m = s + (t - s)/2
      val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
      sum1 + sum2
    }
  }

  val arr = Array(4, -3, 1, 2, 0, -2, 5, -1, 7)
  val p = 2

  pNorm(arr, p)
  pNormTwoPart(arr, p)
  pNormFourPart(arr, p)
  pNormRec(arr, p)

}