package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential should correctly handle the chunk 0 until 0 of an empty array") {
    val res = upsweepSequential(Array[Float](), 0, 0)
    assert(res == 0f)
  }

  test("upsweepSequential should correctly handle the chunk 0 until 1 of an array of 1 element") {
    val res = upsweepSequential(Array[Float](0f), 0, 1)
    assert(res == 0f)
  }

  test("upsweepSequential should correctly handle the chunk 0 until 2 of an array of 2 elements") {
    val res = upsweepSequential(Array[Float](0f, 5f), 0, 2)
    assert(res == 5f)
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 20f), 1, 5, 1)
    assert(res ==
      Node(
        Node(Leaf(1, 2, 1f), Leaf(2, 3, 4f)),
        Node(Leaf(3, 4, 3f), Leaf(4, 5, 5f))
      )
    )
  }

  test("downsweepSequential should correctly handle a 1 element array when the starting angle is zero") {
    val output = new Array[Float](1)
    downsweepSequential(Array[Float](0f), output, 0f, 0, 1)
    assert(output.toList == List(0f))
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweepSequential should correctly handle a 5 element array when the starting angle is 3") {
    val output = new Array[Float](5)
    downsweepSequential(Array[Float](0f, 1f, 8f, 7f, 10f), output, 3f, 1, 4)
    assert(output.toList == List(0f, 3f, 4f, 4f, 0f))
  }

  test("downsweep should correctly handle a 5 element array with a tree for indices 1 until 5 and a starting angle of 3") {
    val tree = Node(
      Node(Leaf(1, 2, 1f), Leaf(2, 3, 4f)),
      Node(Leaf(3, 4, 3f), Leaf(4, 5, 5f))
    )
    val output = new Array[Float](5)
    downsweep(Array[Float](0f, 1f, 8f, 9f, 20f), output, 3f, tree)
    assert(output.toList == List(0f, 3f, 4f, 4f, 5f))
  }

  test("parLineOfSight should correctly handle a 1 element array with threshold 1") {
    val output = new Array[Float](1)
    parLineOfSight(Array[Float](0f), output, 1)
    assert(output.toList == List(0f))
  }

  test("parLineOfSight should correctly handle a 2 element array with threshold 1") {
    val output = new Array[Float](2)
    parLineOfSight(Array[Float](0f, 5f), output, 1)
    assert(output.toList == List(0f, 5f))
  }

  test("parLineOfSight should correctly handle a 2 element array with negative value and threshold 1") {
    val output = new Array[Float](2)
    parLineOfSight(Array[Float](0f, -5f), output, 1)
    assert(output.toList == List(0f, 0f))
  }

  test("parLineOfSight should correctly handle a 5 element array with threshold 1") {
    val output = new Array[Float](5)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 20f), output, 1)
    assert(output.toList == List(0f, 1f, 4f, 4f, 5f))
  }

  test("parLineOfSight should correctly handle a 10 element array with threshold 3") {
    val output = new Array[Float](10)
    parLineOfSight(Array[Float](0f, 1f, 3f, 3f, 8f, 7f, 15f, 17, 15f, 0f), output, 3)
    assert(output.toList == List(0f, 1f, 1.5f, 1.5f, 2f, 2f, 2.5f, 2.5f, 2.5f, 2.5f))
  }

}

