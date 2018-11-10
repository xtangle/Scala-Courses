package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  def testThreshold: Threshold = (_: Int, _: List[Int]) => false

  test("countChange and parCountChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) = {
      assert(countChange(money, coins) == 0,
        s"countChange($money, _) should be 0")
      assert(parCountChange(money, coins, testThreshold) == 0,
        s"parCountChange($money, _) should be 0")
    }

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange and parCountChange should return 1 when money == 0") {
    def check(coins: List[Int]) = {
      assert(countChange(0, coins) == 1,
        s"countChange(0, _) should be 1")
      assert(parCountChange(0, coins, testThreshold) == 1,
        s"parCountChange(0, _) should be 1")
    }

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange and parCountChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) = {
      assert(countChange(money, List()) == 0,
        s"countChange($money, List()) should be 0")
      assert(parCountChange(money, List(), testThreshold) == 0,
        s"parCountChange($money, List()) should be 0")
    }

    check(1)
    check(Int.MaxValue)
  }

  test("countChange and parCountChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) = {
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")
      assert(parCountChange(money, coins, testThreshold) == expected,
        s"parCountChange($money, $coins) should be $expected")
    }

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange and parCountChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) = {
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")
      assert(parCountChange(money, coins, testThreshold) == expected,
        s"parCountChange($money, $coins) should be $expected")
    }

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

}
