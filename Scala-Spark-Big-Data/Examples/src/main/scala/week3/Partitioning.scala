package week3

import common._
import org.apache.spark.{RangePartitioner, SparkContext}
import week3.CFFPurchase._

object Partitioning {

  def main(args: Array[String]): Unit = {
    runSparkContext("Shuffling", (sc: SparkContext) => {

      val purchasesRdd = sc.parallelize(purchases)
      val pairs = purchasesRdd.map(p => (p.customerId, p.price))
      val tunedPartitioner = new RangePartitioner(8, pairs)

      val partitioned = pairs.partitionBy(tunedPartitioner).persist()

      val purchasesPerCust = partitioned.map(p => (p._1, (1, p._2)))

      val purchasesPerMonth = purchasesPerCust
        .reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))
        .collectAsMap()

      highlight {
        println(purchasesPerMonth)
      }

    })
  }

}
