package timeusage

import org.apache.spark.sql.types._
import org.apache.spark.sql._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  val fixture = TimeUsage
  val spark: SparkSession = TimeUsage.spark

  import spark.implicits._

  val primaryActivities = List("t01", "t03", "t11", "t1801", "t1803")
  val workingActivities = List("t05", "t1805")
  val otherActivities = List("t02", "t04", "t06", "t07", "t08", "t09",
    "t10", "t12", "t13", "t14", "t15", "t16", "t18")

  val primaryCols: List[Column] = primaryActivities.map(new Column(_))
  val workingCols: List[Column] = workingActivities.map(new Column(_))
  val otherCols: List[Column] = otherActivities.map(new Column(_))

  val testDF: DataFrame = createTestDF().cache()
  val testSummaryDF: DataFrame = createTestSummaryDF().cache()
  val testSummaryTypedDS: Dataset[TimeUsageRow] = createTestSummaryTypedDS().cache()

  override protected def afterAll(): Unit = {
    if (!spark.sparkContext.isStopped) {
      spark.sparkContext.cancelAllJobs()
      spark.stop()
    }
  }

  /**
    * Using DataFrames
    */
  describe("using DataFrames") {

    it("should create the correct StructType") {
      val testColNames = List("Col_1", "Col_2", "Col_3", "Col_4")
      val expected = StructType(Seq(
        StructField("Col_1", StringType, nullable = false),
        StructField("Col_2", DoubleType, nullable = false),
        StructField("Col_3", DoubleType, nullable = false),
        StructField("Col_4", DoubleType, nullable = false)
      ))
      val actual = fixture.dfSchema(testColNames)

      actual shouldBe expected
    }

    it("should create a Row given raw text values") {
      val testLine = List(" Value one  ", "2.22", "  3.33", "4.44  ")
      val expected = Row("Value one", 2.22, 3.33, 4.44)
      val actual = fixture.row(testLine)

      actual shouldBe expected
    }

    it("should correctly classify activities by their groups") {
      val unknownActivities = List("xxx", "yyy", "zzz")
      val activities = Random.shuffle(primaryActivities ++ workingActivities ++ otherActivities ++ unknownActivities)
      val expected = (primaryCols, workingCols, otherCols)
      val actual = fixture.classifiedColumns(activities)

      actual._1 should contain theSameElementsAs expected._1
      actual._2 should contain theSameElementsAs expected._2
      actual._3 should contain theSameElementsAs expected._3
    }

    it("should correctly construct the 'working' column in the summary DataFrame") {
      val expected = List("working", "working", "not working", "not working", "not working", "working")
      val actual = fixture.timeUsageSummary(primaryCols, workingCols, otherCols, testDF)
        .select("working")
        .collect.toList
        .map(_.getString(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'sex' column in the summary DataFrame") {
      val expected = List("male", "female", "male", "male", "female", "female")
      val actual = fixture.timeUsageSummary(primaryCols, workingCols, otherCols, testDF)
        .select("sex")
        .collect.toList
        .map(_.getString(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'age' column in the summary DataFrame") {
      val expected = List("young", "active", "elder", "young", "active", "active")
      val actual = fixture.timeUsageSummary(primaryCols, workingCols, otherCols, testDF)
        .select("age")
        .collect.toList
        .map(_.getString(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'primaryNeeds' column in the summary DataFrame") {
      val expected = List(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
      val actual = fixture.timeUsageSummary(primaryCols, workingCols, otherCols, testDF)
        .select("primaryNeeds")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'work' column in the summary DataFrame") {
      val expected = List(1.5, 1.6, 1.7, 1.8, 1.9, 2.0)
      val actual = fixture.timeUsageSummary(primaryCols, workingCols, otherCols, testDF)
        .select("work")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'other' column in the summary DataFrame") {
      val expected = List(3.0, 3.1, 3.2, 3.3, 3.4, 3.5)
      val actual = fixture.timeUsageSummary(primaryCols, workingCols, otherCols, testDF)
        .select("other")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should sort by 'working', 'sex', and 'age' in the grouped DataFrame") {
      val expectedWorking = List.fill(5)("not working") ++ List.fill(5)("working")
      val expectedSex = List("female", "female", "female", "male", "male", "female", "female", "male", "male", "male")
      val expectedAge = List("active", "elder", "young", "elder", "young", "active", "young", "active", "elder", "young")
      val (actualWorking, actualSex, actualAge) =
        fixture.timeUsageGrouped(testSummaryDF)
          .select("working", "sex", "age")
          .collect.toList
          .map(r => (r.getString(0), r.getString(1), r.getString(2))).unzip3

      actualWorking shouldBe expectedWorking
      actualSex shouldBe expectedSex
      actualAge shouldBe expectedAge
    }

    it("should correctly construct the 'primaryNeeds' column in the grouped DataFrame") {
      val expected = List(0.9, 0.4, 1.0, 0.3, 0.8, 0.9, 0.2, 0.3, 0.7, 0.6)
      val actual = fixture.timeUsageGrouped(testSummaryDF)
        .select("primaryNeeds")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'work' column in the grouped DataFrame") {
      val expected = List(1.9, 1.4, 2.0, 1.3, 1.8, 1.9, 1.2, 1.3, 1.7, 1.6)
      val actual = fixture.timeUsageGrouped(testSummaryDF)
        .select("work")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'other' column in the grouped DataFrame") {
      val expected = List(3.9, 3.4, 4.0, 3.3, 3.8, 3.8, 3.2, 3.3, 3.7, 3.6)
      val actual = fixture.timeUsageGrouped(testSummaryDF)
        .select("other")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }
  }

  /**
    * Using plain SQL queries
    */
  describe("using plain SQL queries") {

    it("should sort by 'working', 'sex', and 'age' in the grouped DataFrame using plain SQL query") {
      val expectedWorking = List.fill(5)("not working") ++ List.fill(5)("working")
      val expectedSex = List("female", "female", "female", "male", "male", "female", "female", "male", "male", "male")
      val expectedAge = List("active", "elder", "young", "elder", "young", "active", "young", "active", "elder", "young")
      val (actualWorking, actualSex, actualAge) =
        fixture.timeUsageGroupedSql(testSummaryDF)
          .select("working", "sex", "age")
          .collect.toList
          .map(r => (r.getString(0), r.getString(1), r.getString(2))).unzip3

      actualWorking shouldBe expectedWorking
      actualSex shouldBe expectedSex
      actualAge shouldBe expectedAge
    }

    it("should correctly construct the 'primaryNeeds' column in the grouped DataFrame using plain SQL query") {
      val expected = List(0.9, 0.4, 1.0, 0.3, 0.8, 0.9, 0.2, 0.3, 0.7, 0.6)
      val actual = fixture.timeUsageGroupedSql(testSummaryDF)
        .select("primaryNeeds")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'work' column in the grouped DataFrame using plain SQL query") {
      val expected = List(1.9, 1.4, 2.0, 1.3, 1.8, 1.9, 1.2, 1.3, 1.7, 1.6)
      val actual = fixture.timeUsageGroupedSql(testSummaryDF)
        .select("work")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }

    it("should correctly construct the 'other' column in the grouped DataFrame using plain SQL query") {
      val expected = List(3.9, 3.4, 4.0, 3.3, 3.8, 3.8, 3.2, 3.3, 3.7, 3.6)
      val actual = fixture.timeUsageGroupedSql(testSummaryDF)
        .select("other")
        .collect.toList
        .map(_.getDouble(0))

      actual shouldBe expected
    }
  }

  /**
    * Using Datasets
    */
  describe("using Datasets") {

    it("should correctly cast the untyped summary DataFrame to a typed Dataset of TimeUsageRows") {
      val expected = StructType(Seq(
        StructField("working", StringType),
        StructField("sex", StringType),
        StructField("age", StringType),
        StructField("primaryNeeds", DoubleType),
        StructField("work", DoubleType),
        StructField("other", DoubleType)
      ))
      val actual = fixture.timeUsageSummaryTyped(testSummaryDF).schema

      actual shouldBe expected
    }

    it("should sort by 'working', 'sex', and 'age' in the grouped Dataset") {
      val expectedWorking = List.fill(5)("not working") ++ List.fill(5)("working")
      val expectedSex = List("female", "female", "female", "male", "male", "female", "female", "male", "male", "male")
      val expectedAge = List("active", "elder", "young", "elder", "young", "active", "young", "active", "elder", "young")
      val (actualWorking, actualSex, actualAge) =
        fixture.timeUsageGroupedTyped(testSummaryTypedDS)
          .collect.toList
          .map(row => (row.working, row.sex, row.age))
          .unzip3

      actualWorking shouldBe expectedWorking
      actualSex shouldBe expectedSex
      actualAge shouldBe expectedAge
    }

    it("should correctly construct the 'primaryNeeds' column in the grouped Dataset") {
      val expected = List(0.9, 0.4, 1.0, 0.3, 0.8, 0.9, 0.2, 0.3, 0.7, 0.6)
      val actual = fixture.timeUsageGroupedTyped(testSummaryTypedDS)
        .collect.toList
        .map(_.primaryNeeds)

      actual shouldBe expected
    }

    it("should correctly construct the 'work' column in the grouped Dataset") {
      val expected = List(1.9, 1.4, 2.0, 1.3, 1.8, 1.9, 1.2, 1.3, 1.7, 1.6)
      val actual = fixture.timeUsageGroupedTyped(testSummaryTypedDS)
        .collect.toList
        .map(_.work)

      actual shouldBe expected
    }

    it("should correctly construct the 'other' column in the grouped Dataset") {
      val expected = List(3.9, 3.4, 4.0, 3.3, 3.8, 3.8, 3.2, 3.3, 3.7, 3.6)
      val actual = fixture.timeUsageGroupedTyped(testSummaryTypedDS)
        .collect.toList
        .map(_.other)

      actual shouldBe expected
    }
  }

  private def createTestDF(): DataFrame = {
    val columnNames = List("telfs", "tesex", "teage") ++ primaryActivities ++ workingActivities ++ otherActivities
    val data = spark.sparkContext.parallelize(List(
      List(1, 1, 15) ++ List(10, 0, 20, 0, 0) ++ List(40, 50) ++ List(50, 0, 0, 0, 60, 0, 0, 0, 70, 0, 0, 0, 0),
      List(2, 2, 36) ++ List(0, 13, 0, 23, 0) ++ List(43, 53) ++ List(0, 52, 0, 0, 0, 62, 0, 0, 0, 72, 0, 0, 0),
      List(4, 1, 56) ++ List(0, 0, 16, 0, 26) ++ List(46, 56) ++ List(0, 0, 54, 0, 0, 0, 64, 0, 0, 0, 74, 0, 0),
      List(0, 1, 22) ++ List(19, 0, 29, 0, 0) ++ List(49, 59) ++ List(0, 0, 0, 56, 0, 0, 0, 66, 0, 0, 0, 76, 0),
      List(3, 2, 23) ++ List(0, 22, 0, 32, 0) ++ List(52, 62) ++ List(0, 0, 0, 0, 58, 0, 0, 0, 68, 0, 0, 0, 78),
      List(1, 3, 55) ++ List(0, 0, 25, 0, 35) ++ List(55, 65) ++ List(60, 0, 0, 0, 70, 0, 0, 0, 80, 0, 0, 0, 0),
      List(5, 1, 44) ++ List(28, 0, 38, 0, 0) ++ List(58, 68) ++ List(0, 62, 0, 0, 0, 72, 0, 0, 0, 82, 0, 0, 0)
    )).map(_.map(_.toDouble))
      .map(Row(_: _*))
    val schema = StructType(columnNames.map(StructField(_, DoubleType, nullable = false)))
    spark.createDataFrame(data, schema)
  }

  private def createTestSummaryDF(): DataFrame = {
    val columnNames = List("working", "sex", "age", "primaryNeeds", "work", "other")
    val data = spark.sparkContext.parallelize(List(
      List("working", "male", "young", 0.0, 1.0, 3.0),
      List("working", "male", "active", 0.1, 1.1, 3.1),
      List("working", "female", "young", 0.2, 1.2, 3.2),
      List("not working", "male", "elder", 0.3, 1.3, 3.3),
      List("not working", "female", "elder", 0.4, 1.4, 3.4),
      List("working", "male", "active", 0.5, 1.5, 3.5),
      List("working", "female", "active", 0.6, 1.6, 3.6),
      List("working", "male", "elder", 0.7, 1.7, 3.7),
      List("not working", "male", "young", 0.8, 1.8, 3.8),
      List("not working", "female", "active", 0.9, 1.9, 3.9),
      List("not working", "female", "young", 1.0, 2.0, 4.0),
      List("working", "female", "active", 1.1, 2.1, 4.09),
      List("working", "male", "young", 1.2, 2.2, 4.2)
    )).map(Row(_: _*))
    val schema = StructType(
      columnNames.take(3).map(StructField(_, StringType, nullable = false)) ++
        columnNames.takeRight(3).map(StructField(_, DoubleType, nullable = false)))
    spark.createDataFrame(data, schema)
  }

  private def createTestSummaryTypedDS(): Dataset[TimeUsageRow] = {
    testSummaryDF.map(row => TimeUsageRow(
      row.getAs[String]("working"),
      row.getAs[String]("sex"),
      row.getAs[String]("age"),
      row.getAs[Double]("primaryNeeds"),
      row.getAs[Double]("work"),
      row.getAs[Double]("other")
    ))
  }

}
