package week4

import common._
import org.apache.commons.cli.MissingArgumentException
import org.apache.spark.sql.types.{IntegerType, StringType, StructField, StructType}
import org.apache.spark.sql.{Row, SparkSession}

import scala.util.{Failure, Success, Try}

object SparkSQL {

  case class Person(id: Int, name: String, age: Int, city: String)

  val persons = List(
    Person(1, "Alice", 18, "Vancouver"),
    Person(2, "Bob", 22, "London"),
    Person(3, "Charles", 41, "New York"),
    Person(4, "Darwin", 27, "San Francisco"),
    Person(5, "Emily", 33, "London"),
    Person(6, "Francis", 31, "Paris"),
    Person(7, "Grace", 49, "Vancouver"),
    Person(8, "Heather", 29, "London")
  )

  def main(args: Array[String]): Unit = {
    runSparkSession("SparkSQL", (ss: SparkSession) => {
      import ss.implicits._
      val sc = ss.sparkContext

      val peopleDF = getCreationType(args) match {
        /** Creating data frame by inferring schema from case class */
        case CreationType.Inferred =>
          sc.parallelize(persons).toDF()

        /** Creating data frame by explicitly defining the schema */
        case CreationType.Explicit =>
          val fields = List(
            StructField("id", IntegerType, nullable = false),
            StructField("name", StringType, nullable = true),
            StructField("age", IntegerType, nullable = true),
            StructField("city", StringType, nullable = true)
          )
          val schema = StructType(fields)
          val rowRDD = sc.textFile(getAbsolutePath("persons.csv"))
            .map(_.split(","))
            .map(attr => Row(attr(0).trim.toInt, attr(1).trim, attr(2).trim.toInt, attr(3).trim))
          ss.createDataFrame(rowRDD, schema)

        /** Creating data frame from a data source by using the read method */
        case CreationType.DataSource =>
          ss.read.json(getAbsolutePath("persons.json"))
      }

      /** Pretty print out the schema and data frame */
      highlight {
        peopleDF.printSchema
        peopleDF.show
      }

      /** Print out only people who are older than 30, in ascending order by city */
      peopleDF.createOrReplaceTempView("people")
      val adultsDF = ss.sql("SELECT * FROM people WHERE age > 30 ORDER BY city")
      val adults = adultsDF.collectAsList()
      highlight {
        adultsDF.show
        println("Adults: " + adults)
      }

    })
  }

  def getCreationType(args: Array[String]): CreationType.CreationType = {
    Try(CreationType.parseName(args(0))) match {
      case Success(v) => v
      case Failure(_) =>
        val msg = "Usage: runMain week4.SparkSQL <creation-type>, where " +
          "<creation-type> is the DataFrame creation type and is one of: " +
          "{'inferred', 'explicit', 'data-source'}"
        print(Console.RED)
        throw new MissingArgumentException(msg)
    }
  }

  object CreationType extends Enumeration {
    type CreationType = Value
    val Inferred, Explicit, DataSource = Value
    def parseName(input: String): CreationType.CreationType =
      withName(input.split('-').map(_.capitalize).mkString)
  }

}
