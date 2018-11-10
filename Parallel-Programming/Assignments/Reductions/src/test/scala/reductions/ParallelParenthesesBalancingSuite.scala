package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  def check(input: String, expected: Boolean, threshold: Int = 1): Unit = {
    assert(balance(input.toArray) == expected,
      s"balance($input) should be $expected")
    assert(parBalance(input.toArray, threshold) == expected,
      s"parBalance($input) should be $expected")
  }

  test("balance and parBalance should work for empty string (threshold = 1)") {
    check("", true)
  }

  test("balance and parBalance should work for string of length 1 (threshold = 1)") {
    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance and parBalance should work for string of length 2 (threshold = 1)") {
    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance should work sequentially when threshold <= 0") {
    check("()", true, 0)
    check(")(", false, 0)
    check("()", true, -1)
    check(")(", false, -1)
  }

  test("parBalance should work when threshold is greater than the length of chars") {
    check("()", true, Int.MaxValue)
    check(")(", false, Int.MaxValue)
  }

  test("parBalance should work for strings of length 4 (threshold = 2)") {
    check("()()", true, 2)
    check("(())", true, 2)
    check("(..)", true, 2)
    check(".().", true, 2)
    check("()..", true, 2)
    check("..()", true, 2)
    check("(.).", true, 2)
    check(".(.)", true, 2)
    check("....", true, 2)

    check("...)", false, 2)
    check("(...", false, 2)
    check(".())", false, 2)
    check("(().", false, 2)
    check(".((.", false, 2)
    check(".)).", false, 2)
    check(".)(.", false, 2)
    check(")..(", false, 2)
    check("())(", false, 2)
    check(")(()", false, 2)
    check(")()(", false, 2)
  }

}