package week4

import common._
import common.runSparkSession
import org.apache.spark.sql.{Encoder, Encoders, SparkSession}
import org.apache.spark.sql.expressions.Aggregator

object DataSets {

  def main(args: Array[String]): Unit = {
    runSparkSession("DataFrames", (ss: SparkSession) => {
      import ss.implicits._

      val keyValues =
        List((3, "Me"), (1, "Thi"), (2, "Se"), (3, "ssa"), (1, "sIsA"),
          (3, "ge:"), (3, "-)"), (2, "cre"), (2, "t"))

      val keyValuesDS = keyValues.toDS

      val res1 = keyValuesDS
        .groupByKey(_._1)
        .reduceGroups((a, b) => (a._1, a._2 + b._2))
        .map(_._2)
        .sort($"_1")
        .collectAsList

      val res2 = keyValuesDS
        .groupByKey(_._1)
        .mapGroups((k, vs) => (k, vs.reduce((v1, v2) => (k, v1._2 + v2._2))))
        .map(_._2)
        .sort($"_1")
        .collectAsList

      val res3 = keyValuesDS
        .groupByKey(_._1)
        .mapGroups((k, vs) => (k, vs.foldLeft("")((acc, p) => acc + p._2)))
        .sort($"_1")
        .collectAsList

      val res4 = keyValuesDS
        .groupByKey(_._1)
        .mapValues(_._2)
        .reduceGroups(_ + _)
        .sort($"value")
        .collectAsList

      /** Using an Aggregator */
      val strConcat = new Aggregator[(Int, String), String, String] {
        override def zero: String = ""
        override def reduce(b: String, a: (Int, String)): String = b + a._2
        override def merge(b1: String, b2: String): String = b1 + b2
        override def finish(reduction: String): String = reduction
        override def bufferEncoder: Encoder[String] = Encoders.STRING
        override def outputEncoder: Encoder[String] = Encoders.STRING
      }.toColumn

      val res5 = keyValuesDS
        .groupByKey(_._1)
        .agg(strConcat.as[String])
        .sort($"value")
        .collectAsList

      highlight {
        println("Result of keyValues reduced by key (method 1): " + res1)
        println("Result of keyValues reduced by key (method 2): " + res2)
        println("Result of keyValues reduced by key (method 3): " + res3)
        println("Result of keyValues reduced by key (method 4): " + res4)
        println("Result of keyValues reduced by key (method 5): " + res5)
      }
    })
  }
}
