package week3

import common._
import org.apache.spark.SparkContext
import week3.CFFPurchase._

object Shuffling {

  def main(args: Array[String]): Unit = {
    runSparkContext("Shuffling", (sc: SparkContext) => {

      val purchasesRdd = sc.parallelize(purchases)
      val purchasesPerMonth =
        purchasesRdd.map(p => (p.customerId, (1, p.price)))
          .reduceByKey((p1, p2) => (p1._1 + p2._1, p1._2 + p2._2))
          .collectAsMap

      highlight {
        println(purchasesPerMonth)
      }

    })
  }

}
