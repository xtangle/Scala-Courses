package barneshut

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import util.BarnesHutTesting

import scala.collection._

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite with BarnesHutTesting {

  // test cases for quad tree

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    verifyLeaf(inserted, 51f, 46.3f, 5f, Seq(b), b.mass, b.x, b.y, 1)
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))
    verifyLeaf(quad, 17.5f, 27.5f, 5f, Seq(b), b.mass, b.x, b.y, 1)
  }

  test("Leaf with 2 bodies") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(234f, 22f, 14f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b1, b2))
    verifyLeaf(quad, 17.5f, 27.5f, 5f, Seq(b1, b2), 357f, 20.6218f, 18.1344f, 2)
  }

  test("Leaf.insert(b) should create a new Leaf when size <= maximumSize") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(234f, 22f, 14f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, minimumSize, Seq(b1))
    val inserted = quad.insert(b2)
    verifyLeaf(inserted, 17.5f, 27.5f, minimumSize, Seq(b1, b2), 357f, 20.6218f, 18.1344f, 2)
  }

  test("Leaf.insert(b) should create a Fork when size > maximumSize") {
    val b1 = new Body(123f, 14f, 30f, 0f, 0f)
    val b2 = new Body(234f, 22f, 27f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 10f, Seq(b1))
    val inserted = quad.insert(b2)

    inserted match {
      case Fork(nw, ne, sw, se) =>
        verifyEmpty(nw, 15f, 25f, 5f)
        verifyLeaf(ne, 20f, 25f, 5f, Seq(b2), b2.mass, b2.x, b2.y, 1)
        verifyLeaf(sw, 15f, 30f, 5f, Seq(b1), b1.mass, b1.x, b1.y, 1)
        verifyEmpty(se, 20f, 30f, 5f)
      case _ =>
        fail(s"$inserted should have been a Fork")
    }
  }

  test("Fork with all quadrants empty") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    verifyFork(quad, 20f, 30f, 10f, 0f, 20f, 30f, 0)
  }

  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    verifyFork(quad, 20f, 30f, 10f, 123f, 18f, 26f, 1)
  }

  test("Fork with 4 leafs") {
    val b1 = new Body(1f, 15f, 25f, 0f, 0f)
    val b2 = new Body(2f, 20f, 25f, 0f, 0f)
    val b3 = new Body(3f, 15f, 30f, 0f, 0f)
    val b4 = new Body(4f, 20f, 30f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1))
    val ne = Leaf(22.5f, 27.5f, 5f, Seq(b2))
    val sw = Leaf(17.5f, 32.5f, 5f, Seq(b3))
    val se = Leaf(22.5f, 32.5f, 5f, Seq(b4))
    val quad = Fork(nw, ne, sw, se)
    verifyFork(quad, 20f, 30f, 10f, 10f, 18f, 28.5f, 4)
  }

  test("Fork.insert(b) should create new Fork (with a new body in se)") {
    val b1 = new Body(3f, 18f, 26f, 0f, 0f)
    val b2 = new Body(7f, 20.5f, 30.5f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    val inserted = quad.insert(b2)
    verifyFork(inserted, 20f, 30f, 10f, 10f, 19.75f, 29.15f, 2)
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees and 0 speed") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    verifyBody(body, 123f, 18f, 26f, 0f, 0f)
  }

  test("Body.updated should update its x and y positions") {
    val b1 = new Body(123f, 18f, 26f, 3f, 5f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    verifyBody(body, 123f, 18f + 3f*delta, 26f + 5f*delta, 3f, 5f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    verifyBody(body, 123f, 18f, 26f, 12.587037f, 0.015557117f)
  }

  test("Body.updated should approximate a Fork that is far away with a single point of mass") {
    val b1 = new Body(123f, 5f, 5f, 0f, 0f)
    val b2 = new Body(222.2f, 47.5f, 42.5f, 0f, 0f)
    val b3 = new Body(333.3f, 55f, 55f, 0f, 0f)
    val nw = Leaf(45f, 45f, 10f, Seq(b2))
    val ne = Empty(55f, 45f, 10f)
    val sw = Empty(45f, 55f, 10f)
    val se = Leaf(55f, 55f, 10f, Seq(b3))
    val quad = Fork(nw, ne, sw, se)

    val body = b1.updated(quad)

    verifyBody(body, 123f, 5f, 5f, 0.094767f, 0.090734f)
  }

  test("Body.updated should recurse on each child quadtree of a Fork that is close by") {
    val b1 = new Body(123f, 50f, 50f, 0f, 0f)
    val b2 = new Body(222.2f, 47.5f, 42.5f, 0f, 0f)
    val b3 = new Body(333.3f, 55f, 55f, 0f, 0f)
    val nw = Leaf(45f, 45f, 10f, Seq(b2))
    val ne = Empty(55f, 45f, 10f)
    val sw = Empty(45f, 55f, 10f)
    val se = Leaf(55f, 55f, 10f, Seq(b3))
    val quad = Fork(nw, ne, sw, se)

    val body = b1.updated(quad)

    verifyBody(body, 123f, 50f, 50f, 3.589321f, 1.340815f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    verifySectorMatrix(sm, (2, 3, Seq(body)))
  }

  test("'SectorMatrix.+=' should add a body at (100,-12) to the bucket containing the closest point inside the boundary " +
    "of a sector matrix of size 96") {
    val body = new Body(5, 100, -12, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    verifySectorMatrix(sm, (7, 0, Seq(body)))
  }

  test("'SectorMatrix.combine' should contain all bodies from both sector matrices") {
    val b1 = new Body(5, 25, 47, 0.1f, 0.1f)
    val b2 = new Body(5, 30, 37, 0.1f, 0.1f)
    val b3 = new Body(5, 13, 97, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm1 += b1
    sm2 += b2
    sm2 += b3
    val sm3 = sm1.combine(sm2)
    verifySectorMatrix(sm3, (2, 3, Seq(b1, b2)), (1, 7, Seq(b3)))
  }

}


