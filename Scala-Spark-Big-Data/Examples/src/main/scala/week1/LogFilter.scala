package week1

import common._
import org.apache.spark.SparkContext

object LogFilter {

  def main(args: Array[String]): Unit = {
    runSparkContext("LogFilter", (sc: SparkContext) => {

      val logsRdd = sc.textFile(getAbsolutePath("logs.txt"))
      val filteredRdd =
        logsRdd.filter(lg => lg.contains("2016-12") && lg.contains("ERROR")).cache()

      val filteredLogs = filteredRdd.collect()
      val filteredLogsCount = filteredRdd.count()
      filteredRdd.unpersist()

      highlight {
        filteredLogs.foreach(println)
        println("Number of matched logs: " + filteredLogsCount)
      }

    })
  }

}
