package wikipedia

import org.apache.spark.rdd.RDD
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite}

@RunWith(classOf[JUnitRunner])
class WikipediaSuite extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def initializeWikipediaRanking(): Boolean =
    try {
      WikipediaRanking
      true
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        ex.printStackTrace()
        false
    }

  override def beforeEach(): Unit = {
    assert(initializeWikipediaRanking(), " -- did you fill in all the values in WikipediaRanking (conf, sc, wikiRdd)?")
  }

  override def afterAll(): Unit = {
    assert(initializeWikipediaRanking(), " -- did you fill in all the values in WikipediaRanking (conf, sc, wikiRdd)?")
    import WikipediaRanking._
    sc.stop()
  }

  // Conditions:
  // (1) the language stats contain the same elements
  // (2) they are ordered (and the order doesn't matter if there are several languages with the same count)
  def assertEquivalentAndOrdered(given: List[(String, Int)], expected: List[(String, Int)]): Unit = {
    // (1)
    assert(given.toSet === expected.toSet, "The given elements are not the same as the expected elements")
    // (2)
    assert(
      !(given zip given.tail).exists({ case ((_, occ1), (_, occ2)) => occ1 < occ2 }),
      "The given elements are not in descending order"
    )
  }

  test("'occurrencesOfLang' should work for (specific) RDD with no elements") {
    import WikipediaRanking._
    val rdd: RDD[WikipediaArticle] = sc.parallelize(Seq())
    assert(occurrencesOfLang("Java", rdd) === 0)
  }

  test("'occurrencesOfLang' should work for (specific) RDD with one element") {
    import WikipediaRanking._
    val rdd = sc.parallelize(Seq(WikipediaArticle("title", "Java Jakarta")))
    val res = occurrencesOfLang("Java", rdd) == 1
    assert(res, "occurrencesOfLang given (specific) RDD with one element should equal to 1")
  }

  test("'occurrencesOfLang' should work for (specific) RDD with multiple elements") {
    import WikipediaRanking._
    val rdd = sc.parallelize(Seq(
      WikipediaArticle("title1", "Python Pro"),
      WikipediaArticle("title2", "Java Jakarta"),
      WikipediaArticle("title3", "Scaling Scala"),
      WikipediaArticle("title4", "Janky Java")))
    assert(occurrencesOfLang("Java", rdd) === 2)
    assert(occurrencesOfLang("Scala", rdd) === 1)
    assert(occurrencesOfLang("Javascript", rdd) === 0)
  }

  test("'rankLangs' should work for empty list of langs") {
    import WikipediaRanking._
    val langs = List()
    val rdd = sc.parallelize(List(WikipediaArticle("1", "Scala is great"), WikipediaArticle("2", "Java is OK, but Scala is cooler")))
    val ranked = rankLangs(langs, rdd)
    assert(ranked.isEmpty)
  }

  test("'rankLangs' should work for RDD with two elements and sort by descending order") {
    import WikipediaRanking._
    val langs = List("Scala", "Java")
    val rdd = sc.parallelize(List(
      WikipediaArticle("1", "Scala is great"),
      WikipediaArticle("2", "Java is OK, but Scala is cooler")))
    val ranked = rankLangs(langs, rdd)
    assertEquivalentAndOrdered(ranked, List(("Scala", 2), ("Java", 1)))
  }

  test("'rankLangs' should work for RDD with four elements and sort by descending order") {
    import WikipediaRanking._
    val langs = List("Scala", "Java", "Python", "HTML")
    val rdd = sc.parallelize(List(
      WikipediaArticle("1", "Scala is great"),
      WikipediaArticle("2", "Java is OK, but Scala is cooler"),
      WikipediaArticle("3", "Javascript is for Hippies, and same for Python"),
      WikipediaArticle("4", "Nerds and computer scientists use Python and C++")))
    val ranked = rankLangs(langs, rdd)
    assertEquivalentAndOrdered(ranked, List(("Scala", 2), ("Python", 2), ("Java", 1), ("HTML", 0)))
  }

  test("'makeIndex' for an empty list of langs") {
    import WikipediaRanking._
    val langs = List()
    val wa1 = WikipediaArticle("1", "Groovy is pretty interesting, and so is Erlang")
    val wa2 = WikipediaArticle("2", "Scala and Java run on the JVM")
    val wa3 = WikipediaArticle("3", "Scala is not purely functional")
    val articles = List(wa1, wa2, wa3)
    val rdd = sc.parallelize(articles)
    val index = makeIndex(langs, rdd)

    assert(index.count() == 0, "Index should be empty")
  }

  test("'makeIndex' creates a simple index with two entries") {
    import WikipediaRanking._
    val langs = List("Scala", "Java")
    val wa1 = WikipediaArticle("1", "Groovy is pretty interesting, and so is Erlang")
    val wa2 = WikipediaArticle("2", "Scala and Java run on the JVM")
    val wa3 = WikipediaArticle("3", "Scala is not purely functional")
    val articles = List(wa1, wa2, wa3)
    val rdd = sc.parallelize(articles)
    val index = makeIndex(langs, rdd)

    assert(index.collect().toSet === Set(("Scala", Iterable(wa2, wa3)), ("Java", Iterable(wa2))))
  }

  test("'makeIndex' creates a index with three entries, where one entry doesn't appear in any article") {
    import WikipediaRanking._
    val langs = List("Scala", "Java", "LOLCODE")
    val wa1 = WikipediaArticle("1", "Scala is pretty interesting, and so is LOLCODE")
    val wa2 = WikipediaArticle("2", "Jython and Scala run on the JVM, LOLCODE does not")
    val wa3 = WikipediaArticle("3", "What is memes? LOLCODE does")
    val articles = List(wa1, wa2, wa3)
    val rdd = sc.parallelize(articles)
    val index = makeIndex(langs, rdd)

    assert(index.collect().toSet === Set(("Scala", Iterable(wa1, wa2)), ("LOLCODE", Iterable(wa1, wa2, wa3))))
  }

  test("'rankLangsUsingIndex' should work for an empty index") {
    import WikipediaRanking._
    val index: RDD[(String, Iterable[WikipediaArticle])] = sc.parallelize(Seq())
    val ranked = rankLangsUsingIndex(index)
    assert(ranked.isEmpty, "rankLangsUsingIndex result should be empty")
  }

  test("'rankLangsUsingIndex' should work for a simple RDD with three elements") {
    import WikipediaRanking._
    val wa1 = WikipediaArticle("1", "Groovy is pretty interesting, and so is Erlang")
    val wa2 = WikipediaArticle("2", "Scala and Java run on the JVM")
    val wa3 = WikipediaArticle("3", "Scala is not purely functional")
    val index: RDD[(String, Iterable[WikipediaArticle])] =
      sc.parallelize(Seq(("Scala", Iterable(wa2, wa3)), ("Java", Iterable(wa2))))
    val ranked = rankLangsUsingIndex(index)
    assertEquivalentAndOrdered(ranked, List(("Scala", 2), ("Java", 1)))
  }

  test("'rankLangsUsingIndex' should work for a simple RDD and an index containing three langs") {
    import WikipediaRanking._
    val wa1 = WikipediaArticle("1", "Scala is pretty interesting, and so is LOLCODE")
    val wa2 = WikipediaArticle("2", "Jython and Scala run on the JVM, LOLCODE does not")
    val wa3 = WikipediaArticle("3", "What is memes? LOLCODE does")
    val index: RDD[(String, Iterable[WikipediaArticle])] =
      sc.parallelize(Seq(("Scala", Iterable(wa1, wa2)), ("Jython", Iterable(wa2)),
        ("LOLCODE", Iterable(wa1, wa2, wa3))))
    val ranked = rankLangsUsingIndex(index)
    assertEquivalentAndOrdered(ranked, List(("LOLCODE", 3), ("Scala", 2), ("Jython", 1)))
  }

  test("'rankLangsReduceByKey' should work for an empty list of langs") {
    import WikipediaRanking._
    val langs = List()
    val articles = List(
      WikipediaArticle("1","Groovy is pretty interesting, and so is Erlang"),
      WikipediaArticle("2","Scala and Java run on the JVM"),
      WikipediaArticle("3","Scala is not purely functional")
    )
    val rdd = sc.parallelize(articles)
    val ranked = rankLangsReduceByKey(langs, rdd)
    assert(ranked.isEmpty, "rankLangsReduceByKey result should be empty")
  }

  test("'rankLangsReduceByKey' should work for a simple RDD and four langs") {
    import WikipediaRanking._
    val langs = List("Scala", "Java", "Groovy", "Haskell", "Erlang")
    val articles = List(
        WikipediaArticle("1","Groovy is pretty interesting, and so is Erlang"),
        WikipediaArticle("2","Scala and Java run on the JVM"),
        WikipediaArticle("3","Scala is not purely functional"),
        WikipediaArticle("4","The cool kids like Haskell more than Java"),
        WikipediaArticle("5","Java is for enterprise developers")
      )
    val rdd = sc.parallelize(articles)
    val ranked = rankLangsReduceByKey(langs, rdd)
    assertEquivalentAndOrdered(ranked,
      List(("Java", 3), ("Scala", 2), ("Groovy", 1), ("Haskell", 1), ("Erlang", 1)))
  }

}
