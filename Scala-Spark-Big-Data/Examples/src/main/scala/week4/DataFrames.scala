package week4

import common._
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._

object DataFrames {

  def main(args: Array[String]): Unit = {
    runSparkSession("DataFrames", (ss: SparkSession) => {
      import ss.implicits._

      val abosDF = ss.read.json(getAbsolutePath("abos.json"))
      val locationsDF = ss.read.json(getAbsolutePath("locations.json"))

      /** Collect customers that have a subscription and where there is location info */
      val res1 = abosDF
        .filter($"subscription".isNotNull)
        .join(locationsDF, abosDF("id") === locationsDF("id"))
        .select(abosDF("*")) // Select only columns that were in the abos dataframe
        .dropDuplicates
        .orderBy("id")
        .collectAsList

      /** Collect customers that have a subscription for which we don't have location info for */
      val res2 = abosDF
        .join(locationsDF, abosDF("id") === locationsDF("id"), "leftouter")
        .filter(locationsDF("id").isNull)
        .select(abosDF("*"))
        .dropDuplicates
        .orderBy("id")
        .collectAsList

      /** Find out the average price of each type of subscription */
      val res3 = abosDF
        .filter($"subscription".isNotNull)
        .select("subscription.type", "subscription.price")
        .groupBy("type")
        .agg(avg("price").alias("avg_price"))
        .orderBy($"avg_price".desc)
        .collectAsList

      highlight {
        println("Customers that have a subscription and where there is location info: " + res1)
        println("Customers that have a subscription for which we don't have location info for: " + res2)
        println("Average price of each type of subscription: " + res3)
      }
    })
  }

}
