package week1

import common._
import org.apache.spark.SparkContext

object HelloWorld {

  def main(args: Array[String]): Unit = {
    runSparkContext("HelloWorld", (sc: SparkContext) => {

      val rdd = sc.parallelize(Seq("Hello", "World!"))
      val counts = rdd.flatMap(_.toCharArray)
        .map(letter => (letter, 1))
        .reduceByKey(_ + _)
        .collectAsMap()

      highlight {
        println(counts)
      }

    })
  }

}
