package barneshut

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import org.scalatest.junit.JUnitRunner
import util.BarnesHutTesting

import scala.collection.Seq
import scala.collection.parallel.ExecutionContextTaskSupport

@RunWith(classOf[JUnitRunner])
class SimulatorSuite extends FunSuite with BarnesHutTesting with BeforeAndAfterEach {

  var fixture: Simulator = _
  val defaultTaskSupport = new ExecutionContextTaskSupport
  val defaultTimeStats = new TimeStatistics

  trait SimulatorTest {
    val boundary = new Boundaries()
    boundary.minX = 1f
    boundary.minY = 1f
    boundary.maxX = 97f
    boundary.maxY = 97f
  }

  override def beforeEach() {
    fixture = new Simulator(defaultTaskSupport, defaultTimeStats)
  }

  test("Simulator.updateBoundaries should not update the boundary if it contains the body") {
    new SimulatorTest {
      val b = new Body(5f, 72f, 1f, 0.1f, 0.1f)
      val updated: Boundaries = fixture.updateBoundaries(boundary, b)
      assert(updated == boundary, "Should have updated the existing boundary and not have created a new one")
      verifyBoundary(updated, 1f, 1f, 97f, 97f)
    }
  }

  test("Simulator.updateBoundaries should update the boundary if it does not contain the body") {
    new SimulatorTest {
      val b = new Body(5f, 111f, -4.5f, 0.1f, 0.1f)
      val updated: Boundaries = fixture.updateBoundaries(boundary, b)
      assert(updated == boundary, "Should have updated the existing boundary and not have created a new one")
      verifyBoundary(updated, 1f, -4.5f, 111f, 97f)
    }
  }

  test("Simulator.mergeBoundaries should merge the two boundaries") {
    new SimulatorTest {
      val boundary2 = new Boundaries()
      boundary2.minX = -5.6f
      boundary2.minY = 22.1f
      boundary2.maxX = 66f
      boundary2.maxY = 121.1f
      val merged: Boundaries = fixture.mergeBoundaries(boundary, boundary2)
      assert(merged != boundary && merged != boundary2, "Should have created a new boundary and not have updated an " +
        "existing one")
      verifyBoundary(merged, -5.6f, 1f, 97f, 121.1f)
    }
  }

  test("Simulator.computeSectorMatrix should create the correct sector matrix") {
    new SimulatorTest {
      val b1 = new Body(5f, 60f, 23f, 0.1f, 0.1f)
      val b2 = new Body(5f, 48f, 54f, 0.1f, 0.1f)
      val b3 = new Body(5f, 1f, 97f, 0.1f, 0.1f)
      val b4 = new Body(5f, 97f, 1f, 0.1f, 0.1f)
      val sm: SectorMatrix = fixture.computeSectorMatrix(Seq(b1, b2, b3, b4), boundary)
      verifySectorMatrix(sm, (4, 1, Seq(b1)), (3, 4, Seq(b2)), (0, 7, Seq(b3)), (7, 0, Seq(b4)))
    }
  }

  test("Simulator.updateBodies should update each body in the sequence") {
    val b1 = new Body(123f, 50f, 50f, 0f, 0f)
    val b2 = new Body(222.2f, 47.5f, 42.5f, 200f, -300f)
    val b3 = new Body(333.3f, 55f, 55f, -400f, 500f)
    val nw = Leaf(45f, 45f, 10f, Seq(b1, b2))
    val ne = Empty(55f, 45f, 10f)
    val sw = Empty(45f, 55f, 10f)
    val se = Leaf(55f, 55f, 10f, Seq(b3))
    val quad = Fork(nw, ne, sw, se)
    val updated = fixture.updateBodies(Seq(b1, b2, b3), quad)

    updated match {
      case Seq(nb1, nb2, nb3) =>
        verifyBody(nb1, 123f, 50f, 50f, 3.589321f, 1.340815f)
        verifyBody(nb2, 222.2f, 49.5f, 39.5f, 201.4293f, -296.78802f)
        verifyBody(nb3, 333.3f, 51f, 60f, -402.27747f, 497.3639f)
      case _ => fail(s"updateBodies returned an incorrect number of bodies (${updated.length})")
    }
  }

}
