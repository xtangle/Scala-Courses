import java.io.File

import org.apache.spark.sql.SparkSession
import org.apache.spark.{SparkConf, SparkContext}

package object common {

  def runSparkContext(appName: String, block: => (SparkContext => Unit)): Unit = {
    val conf = new SparkConf().setAppName(appName).setMaster("local[4]")
    val sc: SparkContext = new SparkContext(conf)

    try {
      block(sc)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
    } finally {
      stopSparkContext(sc)
    }
  }

  def runSparkSession(appName: String, block: => (SparkSession => Unit)): Unit = {
    val conf = new SparkConf().setAppName(appName).setMaster("local[4]")
    val ss: SparkSession = SparkSession.builder().config(conf).getOrCreate()

    try {
      block(ss)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
    } finally {
      stopSparkSession(ss)
    }
  }

  def stopSparkContext(sc: SparkContext): Unit = {
    if (!sc.isStopped) {
      for ((_, rdd) <- sc.getPersistentRDDs) {
        rdd.unpersist()
      }
      sc.cancelAllJobs()
      sc.stop()
    }
  }

  def stopSparkSession(ss: SparkSession): Unit = stopSparkContext(ss.sparkContext)

  def highlight(printStmts: => Unit): Unit = highlight(Console.GREEN)(printStmts)

  def highlight(color: String)(printStmts: => Unit): Unit = {
    print(color)
    printStmts
    print(Console.WHITE)
  }

  def getAbsolutePath(file: String): String = {
    new File(getClass.getClassLoader.getResource(file).toURI).getAbsolutePath
  }

}
