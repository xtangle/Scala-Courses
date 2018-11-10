package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i, level = 0
    while (i < chars.length && level >= 0) {
      level += (chars(i) match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      })
      i += 1
    }
    level == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /** Implementation of traverse using while loop */
    def traverse(idx: Int, until: Int): (Int, Int) = {
      var i = idx
      var left, right = 0
      while (i < until) {
        chars(i) match {
          case '(' => right += 1
          case ')' =>
            if (right > 0) right -= 1
            else left += 1
          case _ =>
        }
        i += 1
      }
      (left, right)
    }

    /*
    /** Implementation of traverse using tail recursion */
    def traverse(idx: Int, until: Int): (Int, Int) =
      traverseRec(idx: Int, until: Int, 0, 0)

    @tailrec
    def traverseRec(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else {
        val (dl, dr) = chars(idx) match {
          case '(' => (0, 1)
          case ')' => if (arg2 > 0) (0, -1) else (1, 0)
          case _ => (0, 0)
        }
        traverseRec(idx + 1, until, arg1 + dl, arg2 + dr)
      }
    }
    */

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until)
      } else {
        val m = from + (until - from) / 2
        val ((ll, lr), (rl, rr)) =
          parallel(reduce(from, m), reduce(m, until))
        val diff = lr - rl
        if (diff > 0) (ll, rr + diff)
        else (ll - diff, rr)
      }
    }

    val result =
      if (threshold <= 0) traverse(0, chars.length)
      else reduce(0, chars.length)
    result == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
