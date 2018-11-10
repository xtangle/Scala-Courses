package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Signal("hello world"))
    assert(result() === MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Signal(tooLong))
    assert(result2() === MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a variable signal") {
    val signal = Var("hello world")
    val result = TweetLength.tweetRemainingCharsCount(signal)
    assert(result() === MaxTweetLength - tweetLength("hello world"))

    signal() = "goodbye world"
    assert(result() === MaxTweetLength - tweetLength("goodbye world"))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Signal("foo blabla \uD83D\uDCA9 bar"))
    assert(result() === MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Signal(52))
    assert(resultGreen1() === "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Signal(15))
    assert(resultGreen2() === "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Signal(12))
    assert(resultOrange1() === "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Signal(0))
    assert(resultOrange2() === "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Signal(-1))
    assert(resultRed1() === "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Signal(-5))
    assert(resultRed2() === "red")
  }

  test("colorForRemainingCharsCount with a variable signal") {
    val signal = Var(42)
    val result = TweetLength.colorForRemainingCharsCount(signal)
    assert(result() === "green")

    signal() = 14
    assert(result() === "orange")

    signal() = -1
    assert(result() === "red")
  }

  /****************
   ** POLYNOMIAL **
   ****************/

  test("computeDelta for constant signals") {
    val result = Polynomial.computeDelta(Signal(3.0), Signal(-4.0), Signal(2.0))
    assert(result() === -8.0)
  }

  test("computeDelta for variable signals") {
    val signalA = Var(3.0)
    val signalB = Var(-4.0)
    val signalC = Var(2.0)
    val result = Polynomial.computeDelta(signalA, signalB, signalC)
    assert(result() === -8.0)

    signalA() = -1.0
    assert(result() === 24.0)

    signalB() = 0.0
    assert(result() === 8.0)

    signalC() = -2.5
    assert(result() === -10.0)
  }

  test("computeSolutions for no real solution") {
    val result = Polynomial.computeSolutions(Var(3.0), Var(-4.0), Var(2.0), Var(-8.0))
    assert(result() === Set())
  }

  test("computeSolutions for 1 real solution") {
    val result = Polynomial.computeSolutions(Var(-2.0), Var(4.0), Var(-2.0), Var(0.0))
    assert(result() === Set(1.0))
  }

  test("computeSolutions for 2 real solutions") {
    val result = Polynomial.computeSolutions(Var(0.5), Var(-5.0), Var(8.0), Var(9.0))
    assert(result() === Set(2.0, 8.0))
  }

  test("computeSolutions for variable signals") {
    val signalA = Var(3.0)
    val signalB = Var(-4.0)
    val signalC = Var(2.0)
    val signalDelta = Var(-8.0)
    val result = Polynomial.computeSolutions(signalA, signalB, signalC, signalDelta)
    assert(result() === Set())

    signalA() = 2.0
    signalDelta() = 0.0
    assert(result() === Set(1.0))

    signalB() = 5.0
    signalDelta() = 9.0
    assert(result() === Set(-2.0, -0.5))
  }

  /****************
   ** CALCULATOR **
   ****************/

  trait CalculatorTest {
    val literalA: Literal = Literal(3.0)
    val literalB: Literal = Literal(4.0)

    val signalA: Var[Expr] = Var(literalA)
    val signalB: Var[Expr] = Var(literalB)

    val exprs: Map[String, Signal[Expr]] = Map(
      "a" -> signalA,
      "b" -> Var(Ref("a")),
      "c" -> Var(Plus(signalA(), signalB())),
      "d" -> Var(Minus(signalA(), signalB())),
      "e" -> Var(Times(signalA(), signalB())),
      "f" -> Var(Divide(signalA(), signalB())),
      "g" -> Var(Divide(Literal(1.0), Literal(0.0))),
      "h" -> Var(Ref("j")),
      "i" -> Var(Ref("k")),
      "j" -> Var(Ref("i"))
    )

    def isValueNaN(p: (String, Double)): Boolean = p._2.isNaN
    def isValueDefined(p: (String, Double)): Boolean = !isValueNaN(p)
  }

  test("eval on literal expr") {
    new CalculatorTest {
      assert(Calculator.eval(literalA, exprs) === literalA.v)
    }
  }

  test("eval on ref expr") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("b"), exprs) === literalA.v)
    }
  }

  test("eval on plus expr") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("c"), exprs) === literalA.v + literalB.v)
    }
  }

  test("eval on minus expr") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("d"), exprs) === literalA.v - literalB.v)
    }
  }

  test("eval on times expr") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("e"), exprs) === literalA.v * literalB.v)
    }
  }

  test("eval on divide expr") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("f"), exprs) === literalA.v / literalB.v)
    }
  }

  test("eval on divide expr with 0 denominator") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("g"), exprs).isNaN)
    }
  }

  test("eval on cyclic reference") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("h"), exprs).isNaN)
    }
  }

  test("eval on ref expr not in references") {
    new CalculatorTest {
      assert(Calculator.eval(Ref("z"), exprs).isNaN)
    }
  }

  test("eval on variable signals") {
    new CalculatorTest {
      val a: Double = literalA.v
      val b: Double = literalB.v
      val kRef: Expr = Ref("k")
      val kSig: Var[Expr] = Var(Plus(Ref("c"), Ref("d"))) // k = (a + b) + (a - b)
      var newExprs: Map[String, Signal[Expr]] = exprs + ("k" -> kSig)
      assert(Calculator.eval(kRef, newExprs) === 2*a)

      signalA() = Literal(-a) // k = (-a + b) + (-a - b)
      assert(Calculator.eval(kRef, newExprs) === -2*a)

      signalB() = Literal(1.5*b)
      kSig() = Minus(Ref("c"), Ref("d")) // k = (-a + 1.5b) - (-a - 1.5b) = 3b
      assert(Calculator.eval(kRef, newExprs) === 3*b)
    }
  }

  test("computeValues on constant signals") {
    new CalculatorTest {
      val a: Double = literalA.v
      val b: Double = literalB.v
      val expectedValues = Set(
        "a" -> a,
        "b" -> a,
        "c" -> (a + b),
        "d" -> (a - b),
        "e" -> (a * b),
        "f" -> (a / b),
        "g" -> Double.NaN,
        "h" -> Double.NaN,
        "i" -> Double.NaN,
        "j" -> Double.NaN
      )
      val result: Set[(String, Double)] = Calculator.computeValues(exprs).mapValues(_.apply()).toSet
      assert(result.filter(isValueDefined) === expectedValues.filter(isValueDefined))
      assert(result.filter(isValueNaN).map(p => p._1) === expectedValues.filter(isValueNaN).map(p => p._1))
    }
  }

  test("computeValues on variable signals") {
    new CalculatorTest {
      val a: Double = literalA.v
      val b: Double = literalB.v
      val c: Double = 1.5*literalA.v
      val d: Double = -literalB.v
      val result: Map[String, Signal[Double]] = Calculator.computeValues(exprs)

      signalA() = Literal(c)
      signalB() = Literal(d)
      val expectedValues = Set(
        "a" -> c,
        "b" -> c,
        "c" -> (c + d),
        "d" -> (c - d),
        "e" -> (c * d),
        "f" -> (c / d),
        "g" -> Double.NaN,
        "h" -> Double.NaN,
        "i" -> Double.NaN,
        "j" -> Double.NaN
      )

      val resultAsSet: Set[(String, Double)] = result.mapValues(_.apply()).toSet
      assert(resultAsSet.filter(isValueDefined) === expectedValues.filter(isValueDefined))
      assert(resultAsSet.filter(isValueNaN).map(p => p._1) === expectedValues.filter(isValueNaN).map(p => p._1))
    }
  }

}
