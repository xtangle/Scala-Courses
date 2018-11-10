package kmeans

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  test("'classify' should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()

    checkClassify(points, means, expected)
    checkParClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))

    checkClassify(points, means, expected)
    checkParClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))

    checkClassify(points, means, expected)
    checkParClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))

    checkClassify(points, means, expected)
    checkParClassify(points, means, expected)
  }

  test("'update' should work for empty 'classified' and empty 'oldMeans'") {
    val classified = GenMap[Point, GenSeq[Point]]()
    val oldMeans: GenSeq[Point] = IndexedSeq()
    val expected: GenSeq[Point] = IndexedSeq()

    checkUpdate(classified, oldMeans, expected)
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for 'classified' == GenMap((1, 1, 1) -> ()) and 'oldMeans' == GenSeq((1, 1, 1))") {
    val mean = new Point(1, 1, 1)
    val classified = GenMap[Point, GenSeq[Point]](mean -> GenSeq())
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val expected: GenSeq[Point] = IndexedSeq(mean)

    checkUpdate(classified, oldMeans, expected)
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for 'classified' == GenMap((1, 1, 1) -> ((2, 2, 2))) and 'oldMeans' == GenSeq((1, 1, 1))") {
    val mean = new Point(1, 1, 1)
    val p = new Point(2, 2, 2)
    val classified = GenMap[Point, GenSeq[Point]](mean -> GenSeq(p))
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val expected: GenSeq[Point] = IndexedSeq(p)

    checkUpdate(classified, oldMeans, expected)
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for 'classified' == GenMap((1, 1, 1) -> ((0, 1, 0), (1, 0, 0))) and 'oldMeans' == GenSeq((1, 1, 1))") {
    val mean = new Point(1, 1, 1)
    val p1 = new Point(0, 1, 0)
    val p2 = new Point(1, 0, 0)
    val expectedMean = new Point(0.5, 0.5, 0)
    val classified = GenMap[Point, GenSeq[Point]](mean -> GenSeq(p1, p2))
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val expected: GenSeq[Point] = IndexedSeq(expectedMean)

    checkUpdate(classified, oldMeans, expected)
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for 'classified' == GenMap((-1, -1, 0) -> ((-1, 1, 0), (0, -1, 1)), (0, 1, 1) -> ((0, 1, 0), (0, 0, 1))) " +
    "and 'oldMeans' == GenSeq((-1, -1, 0), (0, 1, 1))") {
    val mean1 = new Point(-1, -1, 0)
    val mean2 = new Point(0, 1, 1)
    val p1 = new Point(-1, 1, 0)
    val p2 = new Point(0, -1, 1)
    val p3 = new Point(0, 1, 0)
    val p4 = new Point(0, 0, 1)
    val expectedMean1 = new Point(-0.5, 0, 0.5)
    val expectedMean2 = new Point(0, 0.5, 0.5)
    val classified = GenMap[Point, GenSeq[Point]](mean1 -> GenSeq(p1, p2), mean2 -> GenSeq(p3, p4))
    val oldMeans: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected: GenSeq[Point] = IndexedSeq(expectedMean1, expectedMean2)

    checkUpdate(classified, oldMeans, expected)
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'converged' should return true for 'eta' == 0 and empty 'oldMeans' and empty 'newMeans'") {
    val eta: Double = 0
    val oldMeans: GenSeq[Point] = IndexedSeq()
    val newMeans: GenSeq[Point] = IndexedSeq()
    val expected = true

    checkConverged(eta, oldMeans, newMeans, expected)
    checkParConverged(eta, oldMeans, newMeans, expected)
  }

  test("'converged' should return true for 'eta' == 0 and 'oldMeans' == GenSeq((0, 0, 0)) and 'newMeans' == GenSeq((0, 0, 0))") {
    val eta: Double = 0
    val p = new Point(1, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(p)
    val newMeans: GenSeq[Point] = IndexedSeq(p)
    val expected = true

    checkConverged(eta, oldMeans, newMeans, expected)
    checkParConverged(eta, oldMeans, newMeans, expected)
  }

  test("'converged' should return true for 'eta' == 3 and 'oldMeans' == GenSeq((1, 1, 1)) and 'newMeans' == GenSeq((0, 0, 0))") {
    val eta: Double = 3
    val p1 = new Point(0, 0, 0)
    val p2 = new Point(1, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(p1)
    val newMeans: GenSeq[Point] = IndexedSeq(p2)
    val expected = true

    checkConverged(eta, oldMeans, newMeans, expected)
    checkParConverged(eta, oldMeans, newMeans, expected)
  }

  test("'converged' should return false for 'eta' == 2 and 'oldMeans' == GenSeq((1, 1, 1)) and 'newMeans' == GenSeq((0, 0, 0))") {
    val eta: Double = 2
    val p1 = new Point(0, 0, 0)
    val p2 = new Point(1, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(p1)
    val newMeans: GenSeq[Point] = IndexedSeq(p2)
    val expected = false

    checkConverged(eta, oldMeans, newMeans, expected)
    checkParConverged(eta, oldMeans, newMeans, expected)
  }

  test("'converged' should return true for 'eta' == 4 and 'oldMeans' == GenSeq((1, 0, 1), (0, 1, 0), (0, 1, -1)) " +
    "and 'newMeans' == GenSeq((-1, 0, 1), (0, -1, 0), (0, 1, 1))") {
    val eta: Double = 4
    val p1 = new Point(1, 0, 1)
    val p2 = new Point(0, 1, 0)
    val p3 = new Point(0, 1, -1)
    val p4 = new Point(-1, 0, 1)
    val p5 = new Point(0, -1, 0)
    val p6 = new Point(0, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(p1, p2, p3)
    val newMeans: GenSeq[Point] = IndexedSeq(p4, p5, p6)
    val expected = true

    checkConverged(eta, oldMeans, newMeans, expected)
    checkParConverged(eta, oldMeans, newMeans, expected)
  }

  test("'converged' should return false for 'eta' == 4 and 'oldMeans' == GenSeq((1, 0, 1), (0, 1, 0), (0, 1, -1)) " +
    "and 'newMeans' == GenSeq((-1, 0, 1), (0, -2, 0), (0, 1, 1))") {
    val eta: Double = 4
    val p1 = new Point(1, 0, 1)
    val p2 = new Point(0, 1, 0)
    val p3 = new Point(0, 1, -1)
    val p4 = new Point(-1, 0, 1)
    val p5 = new Point(0, -2, 0)
    val p6 = new Point(0, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(p1, p2, p3)
    val newMeans: GenSeq[Point] = IndexedSeq(p4, p5, p6)
    val expected = false

    checkConverged(eta, oldMeans, newMeans, expected)
    checkParConverged(eta, oldMeans, newMeans, expected)
  }

  test("'kMeans' should work for empty 'points' and empty 'oldMeans' and 'eta' == 0") {
    val eta: Double = 0
    val points: GenSeq[Point] = IndexedSeq()
    val oldMeans: GenSeq[Point] = IndexedSeq()
    val expected: GenSeq[Point] = IndexedSeq()

    checkKMeans(points, oldMeans, eta, expected)
    checkParKMeans(points, oldMeans, eta, expected)
  }

  test("'kMeans' should work for 'points' == GenSeq((0, 0, 1), (0, 0, -1), (0, 1, 0), (0, 10, 0)) and " +
    "'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12.25") {
    val eta: Double = 12.25
    val p1 = new Point(0, 0, 1)
    val p2 = new Point(0, 0, -1)
    val p3 = new Point(0, 1, 0)
    val p4 = new Point(0, 10, 0)
    val oldMean1 = new Point(0, -1, 0)
    val oldMean2 = new Point(0, 2, 0)
    val expectedMean1 = new Point(0, 0, 0)
    val expectedMean2 = new Point(0, 5.5, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val oldMeans: GenSeq[Point] = IndexedSeq(oldMean1, oldMean2)
    val expected: GenSeq[Point] = IndexedSeq(expectedMean1, expectedMean2)

    checkKMeans(points, oldMeans, eta, expected)
    checkParKMeans(points, oldMeans, eta, expected)
  }

  def checkPoint(actual: Point, expected: Point): Boolean = {
    actual.x == expected.x && actual.y == expected.y && actual.z == expected.z
  }

  def checkSeq(actual: GenSeq[Point], expected: GenSeq[Point]): Boolean = {
    actual.zip(expected).forall{ case(a, e) => checkPoint(a, e)}
  }

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    val actual = classify(points, means)
    assert(actual == expected, s"$actual should equal to $expected")
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    val actual = classify(points.par, means.par)
    assert(actual == expected, s"$actual should equal to $expected")
  }

  def checkUpdate(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point], expected: GenSeq[Point]) {
    assert(checkSeq(update(classified, oldMeans), expected),
      s"update($classified, $oldMeans) should equal to $expected")
  }

  def checkParUpdate(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point], expected: GenSeq[Point]) {
    assert(checkSeq(update(classified.par, oldMeans.par), expected),
      s"update($classified par, $oldMeans par) should equal to $expected")
  }

  def checkConverged(eta: Double, oldMeans: GenSeq[Point], newMeans: GenSeq[Point], expected: Boolean) {
    assert(converged(eta)(oldMeans, newMeans) == expected,
      s"converged($eta)($oldMeans, $newMeans) should be $expected")
  }

  def checkParConverged(eta: Double, oldMeans: GenSeq[Point], newMeans: GenSeq[Point], expected: Boolean) {
    assert(converged(eta)(oldMeans.par, newMeans.par) == expected,
      s"converged($eta)($oldMeans par, $newMeans par) should be $expected")
  }

  def checkKMeans(points: GenSeq[Point], oldMeans: GenSeq[Point], eta: Double, expected: GenSeq[Point]) {
    val actual = kMeans(points, oldMeans, eta)
    assert(checkSeq(actual, expected), s"$actual should be $expected")
  }

  def checkParKMeans(points: GenSeq[Point], oldMeans: GenSeq[Point], eta: Double, expected: GenSeq[Point]) {
    val actual = kMeans(points.par, oldMeans.par, eta)
    assert(checkSeq(actual, expected), s"$actual should be $expected")
  }

}


  
