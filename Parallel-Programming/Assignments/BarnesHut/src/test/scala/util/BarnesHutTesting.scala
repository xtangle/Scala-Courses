package util

import barneshut._
import util.FloatOps._
import org.scalatest.FunSuite

import scala.collection.Seq

trait BarnesHutTesting extends FunSuite {

  // Helper methods

  def verifyEmpty(actual: Quad, centerX: Float, centerY: Float, size: Float): Unit = {
    actual match {
      case Empty(cx, cy, sz) =>
        assert(cx == centerX, s"$cx should be $centerX")
        assert(cy == centerY, s"$cy should be $centerY")
        assert(sz == size, s"$sz should be $size")
      case _ =>
        fail(s"$actual should have been Empty")
    }
  }

  def verifyLeaf(actual: Quad, centerX: Float, centerY: Float, size: Float, bodies: Seq[Body],
                         mass: Float, massX: Float, massY: Float, total: Int): Unit = {
    actual match {
      case Leaf(cx, cy, sz, bds) =>
        assert(cx == centerX, s"$cx should be $centerX")
        assert(cy == centerY, s"$cy should be $centerY")
        assert(sz == size, s"$sz should be $size")
        assert(bds.toSet == bodies.toSet, s"$bds toSet should be $bodies toSet")
        assert(actual.mass ~= mass, s"${actual.mass} should be $mass")
        assert(actual.massX ~= massX, s"${actual.massX} should be $massX")
        assert(actual.massY ~= massY, s"${actual.massY} should be $massY")
        assert(actual.total == total, s"${actual.total} should be $total")
      case _ =>
        fail(s"$actual should have been a Leaf")
    }
  }

  def verifyFork(actual: Quad, centerX: Float, centerY: Float, size: Float,
                         mass: Float, massX: Float, massY: Float, total: Int): Unit = {
    actual match {
      case Fork(_, _, _, _) =>
        assert(actual.centerX == centerX, s"${actual.centerX} should be $centerX")
        assert(actual.centerY == centerY, s"${actual.centerY} should be $centerY")
        assert(actual.mass ~= mass, s"${actual.mass} should be $mass")
        assert(actual.massX ~= massX, s"${actual.massX} should be $massX")
        assert(actual.massY ~= massY, s"${actual.massY} should be $massY")
        assert(actual.total == total, s"${actual.total} should be $total")
      case _ =>
        fail(s"$actual should have been a Fork")
    }
  }

  def verifyBody(actual: Body, mass: Float, x: Float, y: Float,
                         xspeed: Float, yspeed: Float): Unit = {
    assert(actual.mass == mass, s"${actual.mass} should be $mass")
    assert(actual.x ~= x, s"${actual.x} should be $x")
    assert(actual.y ~= y, s"${actual.y} should be $y")
    assert(actual.xspeed ~= xspeed, s"${actual.xspeed} should be $xspeed")
    assert(actual.yspeed ~= yspeed, s"${actual.yspeed} should be $yspeed")
  }

  def verifyBoundary(b: Boundaries, minX: Float, minY: Float, maxX: Float, maxY: Float): Unit = {
    assert(b.minX == minX, s"${b.minX} should be $minX")
    assert(b.minY == minY, s"${b.minY} should be $minY")
    assert(b.maxX == maxX, s"${b.maxX} should be $maxX")
    assert(b.maxY == maxY, s"${b.maxY} should be $maxY")
  }

  def verifySectorMatrix(actual: SectorMatrix,
                         expectedSectors: (Int, Int, Seq[Body])*): Unit = {
    expectedSectors.foreach{ case (x, y, sb) =>
      assert(actual(x, y).toSet == sb.toSet,
        s"Sector ($x, $y) has bodies ${actual(x, y)} rather than the expected bodies $sb")
    }
  }

}
