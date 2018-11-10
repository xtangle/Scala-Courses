package stackoverflow

import org.apache.spark.rdd.RDD
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import stackoverflow.StackOverflow.sc

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {

  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120

    lazy val lines: RDD[String] = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    lazy val raw: RDD[Posting] = rawPostings(lines)
    lazy val grouped: RDD[(Int, Iterable[(Posting, Posting)])] = groupedPostings(raw)
    lazy val scored: RDD[(Posting, Int)] = scoredPostings(grouped)
    lazy val vectors: RDD[(Int, Int)] = vectorPostings(scored)
  }

  override def afterAll(): Unit = {
    for ((_, rdd) <- sc.getPersistentRDDs) {rdd.unpersist()}
    sc.cancelAllJobs()
    sc.stop()
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("groupedPostings should work for an empty rdd") {
    val rdd: RDD[Posting] = sc.parallelize(Seq())
    val grouped = testObject.groupedPostings(rdd)

    verifyGroupedPostings(grouped, Set())
  }

  test("groupedPostings should work for an rdd containing a single question") {
    val p1 = Posting(1, 1, None, None, 4, Some("Java"))
    val rdd: RDD[Posting] = sc.parallelize(Seq(p1))
    val grouped = testObject.groupedPostings(rdd)

    verifyGroupedPostings(grouped, Set())
  }

  test("groupedPostings should work for an rdd containing 5 postings with questions and answers") {
    val p1 = Posting(1, 1, None, None, 4, Some("Java"))
    val p2 = Posting(2, 2, None, Some(4), 10, None)
    val p3 = Posting(2, 3, None, Some(4), 15, None)
    val p4 = Posting(1, 4, Some(3), None, 12, Some("SQL"))
    val p5 = Posting(2, 5, None, Some(1), 2, None)
    val rdd: RDD[Posting] = sc.parallelize(Seq(p1, p2, p3, p4, p5))
    val grouped = testObject.groupedPostings(rdd)

    verifyGroupedPostings(grouped, Set(
      (1, Set((p1, p5))),
      (4, Set((p4, p2), (p4, p3)))
    ))
  }

  test("groupedPostings should work for an rdd containing 5 postings where a question " +
    "does not have an answer") {
    val p1 = Posting(1, 1, None, None, 4, Some("Java"))
    val p2 = Posting(2, 2, None, Some(4), 10, None)
    val p3 = Posting(2, 3, None, Some(4), 15, None)
    val p4 = Posting(1, 4, Some(3), None, 12, Some("SQL"))
    val p5 = Posting(2, 5, None, Some(4), 2, None)
    val rdd: RDD[Posting] = sc.parallelize(Seq(p1, p2, p3, p4, p5))
    val grouped = testObject.groupedPostings(rdd)

    verifyGroupedPostings(grouped, Set(
      (4, Set((p4, p2), (p4, p3), (p4, p5)))
    ))
  }

  test("scoredPostings should work for an empty rdd") {
    val rdd: RDD[(Int, Iterable[(Posting, Posting)])] = sc.parallelize(Seq())
    val scored = testObject.scoredPostings(rdd)

    verifyScoredPostings(scored, Set())
  }

  test("scoredPostings should work for a single question answer pair") {
    val q1 = Posting(1, 1, None, None, 10, Some("Java"))
    val a1 = Posting(2, 2, None, Some(1), 8, None)

    val g1 = (1, Iterable((q1, a1)))
    val rdd: RDD[(Int, Iterable[(Posting, Posting)])] = sc.parallelize(Seq(g1))
    val scored = testObject.scoredPostings(rdd)

    verifyScoredPostings(scored, Set((q1, 8)))
  }

  test("scoredPostings should work for a single question with multiple answers") {
    val q1 = Posting(1, 1, None, None, 10, Some("Java"))
    val a1 = Posting(2, 2, None, Some(1), 5, None)
    val a2 = Posting(2, 3, None, Some(1), 9, None)
    val a3 = Posting(2, 4, None, Some(1), 8, None)

    val g1 = (1, Iterable((q1, a1), (q1, a2), (q1, a3)))
    val rdd: RDD[(Int, Iterable[(Posting, Posting)])] = sc.parallelize(Seq(g1))
    val scored = testObject.scoredPostings(rdd)

    verifyScoredPostings(scored, Set((q1, 9)))
  }

  test("scoredPostings should work for multiple questions with answers") {
    val q1 = Posting(1, 1, None, None, 10, Some("Java"))
    val a1 = Posting(2, 2, None, Some(1), 7, None)
    val a2 = Posting(2, 3, None, Some(1), 5, None)
    val q2 = Posting(1, 4, None, None, 2, Some("Scala"))
    val a3 = Posting(2, 5, None, Some(4), 3, None)
    val a4 = Posting(2, 6, None, Some(4), 4, None)
    val q3 = Posting(1, 7, None, None, 12, Some("C++"))
    val a5 = Posting(2, 8, None, Some(7), 9, None)

    val g1 = (1, Iterable((q1, a1), (q1, a2)))
    val g2 = (4, Iterable((q2, a3), (q2, a4)))
    val g3 = (7, Iterable((q3, a5)))
    val rdd: RDD[(Int, Iterable[(Posting, Posting)])] = sc.parallelize(Seq(g1, g2, g3))
    val scored = testObject.scoredPostings(rdd)

    verifyScoredPostings(scored, Set((q1, 7), (q2, 4), (q3, 9)))
  }

  test("vectorPostings should work on an empty rdd") {
    val rdd: RDD[(Posting, Int)] = sc.parallelize(Seq())
    val scored = testObject.vectorPostings(rdd)

    verifyVectorPostings(scored, Set())
  }

  test("vectorPostings should work on a single posting") {
    val p1 = (Posting(1, 1, None, None, 10, Some("Java")), 9)
    val rdd: RDD[(Posting, Int)] = sc.parallelize(Seq(p1))
    val scored = testObject.vectorPostings(rdd)

    verifyVectorPostings(scored, Set((testObject.langSpread, 9)))
  }

  test("vectorPostings should work on a posting that is not listed in langs") {
    val p1 = (Posting(1, 1, None, None, 10, Some("EssQueEl")), 9)
    val rdd: RDD[(Posting, Int)] = sc.parallelize(Seq(p1))
    val scored = testObject.vectorPostings(rdd)

    verifyVectorPostings(scored, Set())
  }

  test("vectorPostings should work on a posting that has no tag") {
    val p1 = (Posting(1, 1, None, None, 10, None), 9)
    val rdd: RDD[(Posting, Int)] = sc.parallelize(Seq(p1))
    val scored = testObject.vectorPostings(rdd)

    verifyVectorPostings(scored, Set())
  }

  test("vectorPostings should work for multiple postings of the same tag") {
    val p1 = (Posting(1, 1, None, None, 10, Some("Python")), 6)
    val p2 = (Posting(1, 2, Some(9), None, 4, Some("Python")), 9)
    val p3 = (Posting(1, 3, Some(8), None, 18, Some("Python")), 11)
    val rdd: RDD[(Posting, Int)] = sc.parallelize(Seq(p1, p2, p3))
    val scored = testObject.vectorPostings(rdd)

    verifyVectorPostings(scored, Set(
      (3 * testObject.langSpread, 6),
      (3 * testObject.langSpread, 9),
      (3 * testObject.langSpread, 11)
    ))
  }

  test("vectorPostings should work for multiple postings of different tags") {
    val p1 = (Posting(1, 1, None, None, 7, Some("Scala")), 9)
    val p2 = (Posting(1, 2, Some(11), None, 14, Some("Python")), 10)
    val p3 = (Posting(1, 3, Some(12), None, 3, Some("Java")), 4)
    val p4 = (Posting(1, 4, None, None, 5, Some("EssQueEl")), 9)
    val p5 = (Posting(1, 5, Some(13), None, 11, Some("Scala")), 8)
    val p6 = (Posting(1, 6, Some(14), None, 8, None), 15)
    val rdd: RDD[(Posting, Int)] = sc.parallelize(Seq(p1, p2, p3, p4, p5, p6))
    val scored = testObject.vectorPostings(rdd)

    verifyVectorPostings(scored, Set(
      (10 * testObject.langSpread, 9),
      (3 * testObject.langSpread, 10),
      (testObject.langSpread, 4),
      (10 * testObject.langSpread, 8)
    ))
  }

  /**
    * Tests on real data (time consuming)
    */

  test("scoredPostings on real data should have the correct number of postings") {
    assert(testObject.scored.count === 2121822)
  }

  test("scoredPostings on real data should contain the tuples specified in the assignment") {
    val expectedTuples = Seq(
      (Posting(1, 6, None, None, 140, Some("CSS")), 67),
      (Posting(1, 42, None, None, 155, Some("PHP")), 89),
      (Posting(1, 72, None, None, 16, Some("Ruby")), 3),
      (Posting(1, 126, None, None, 33, Some("Java")), 30),
      (Posting(1, 174, None, None, 38, Some("C#")), 20)
    )
    val expectedTuplesRdd: RDD[(Posting, Int)] = sc.parallelize(expectedTuples)
    val intersection: RDD[(Posting, Int)] = expectedTuplesRdd.intersection(testObject.scored)

    assert(intersection.collect.toSet === expectedTuples.toSet,
      "Scored rdd does not contain the expected tuples")
  }

  test("vectorPostings on real data should contain the tuples specified in the assignment") {
    val expectedTuples = Seq(
      (350000, 67),
      (100000, 89),
      (300000, 3),
      (50000, 30),
      (200000, 20)
    )
    val expectedTuplesRdd: RDD[(Int, Int)] = sc.parallelize(expectedTuples)
    val intersection: RDD[(Int, Int)] = expectedTuplesRdd.intersection(testObject.vectors)

    assert(intersection.collect.sorted === expectedTuples.sorted,
      "Vector rdd does not contain the expected tuples")
  }

  private def verifyGroupedPostings(actual: RDD[(Int, Iterable[(Posting, Posting)])],
                                    expected: Set[(Int, Set[(Posting, Posting)])]) = {
    assert(actual.collect().toSet
      .map((grp: (Int, Iterable[(Posting, Posting)])) =>
        (grp._1, grp._2.toSet)) === expected)
  }

  private def verifyScoredPostings(actual: RDD[(Posting, Int)],
                                   expected: Set[(Posting, Int)]) = {
    assert(actual.collect().toSet === expected)
  }

  private def verifyVectorPostings(actual: RDD[(Int, Int)],
                                   expected: Set[(Int, Int)]) = {
    assert(actual.collect().toSet === expected)
  }

}
