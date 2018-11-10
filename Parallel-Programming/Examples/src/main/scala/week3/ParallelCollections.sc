import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.{GenSeq, GenSet, mutable}

object ParallelCollections {

  /**
    * Largest palindrome example
    **/
  def largestPalindrome(xs: GenSeq[Int]): Int = {
    xs.aggregate(Int.MinValue)(
      (largest: Int, n: Int) =>
        if (n > largest && n.toString == n.toString.reverse) n else largest,
      math.max
    )
  }

  largestPalindrome((0 until 999999).toArray)

  /**
    * Set intersection example
    **/
  // Doesn't work correctly for parallel collections!
  def intersection1(a: GenSet[Int], b: GenSet[Int]) = {
    val result = mutable.Set[Int]()
    for (x <- a) if (b contains x) result += x
    result
  }

  // Corrected version
  def intersection2(a: GenSet[Int], b: GenSet[Int]) = {
    val result = new ConcurrentSkipListSet[Int]()
    for (x <- a) if (b contains x) result.add(x)
    result
  }

  // Another correct solution by avoiding side-effects
  def intersection3(a: GenSet[Int], b: GenSet[Int]) = {
    if (a.size < b.size) a.filter(b(_))
    else b.filter(a(_))
  }

  val range1: Range = 0 until 1000
  val range2: Range = 0 until 1000 by 4

  intersection1(range1.toSet, range2.toSet).size
  intersection1(range1.par.toSet, range2.par.toSet).size

  intersection2(range1.toSet, range2.toSet).size
  intersection2(range1.par.toSet, range2.par.toSet).size

  intersection3(range1.toSet, range2.toSet).size
  intersection3(range1.par.toSet, range2.par.toSet).size

}