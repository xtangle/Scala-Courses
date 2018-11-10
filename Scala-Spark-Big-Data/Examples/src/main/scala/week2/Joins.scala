package week2

import common._
import org.apache.spark.SparkContext

object Joins {

  val as: List[(Int, (String, Subscription))] = List(
    (101, ("Ruetli", AG)),
    (102, ("Brelaz", DemiTarif)),
    (103, ("Gress", DemiTarifVisa)),
    (104, ("Schatten", DemiTarif)))

  val ls: List[(Int, String)] = List(
    (101, "Bern"), (101, "Thun"),
    (102, "Lausanne"), (102, "Geneve"), (102, "Nyon"),
    (103, "Zurich"), (103, "St-Gallen"), (103, "Chur"))

  def main(args: Array[String]): Unit = {
    runSparkContext("Joins", (sc: SparkContext) => {

      val abos = sc.parallelize(as).cache
      val locations = sc.parallelize(ls).cache

      val trackedCustomers = abos.join(locations).collect
      highlight {
        println("Tracked customers: ")
        trackedCustomers.foreach(println)
      }

      val abosWithOptionalLocations = abos.leftOuterJoin(locations).collect
      highlight {
        println("Abos with optional locations: ")
        abosWithOptionalLocations.foreach(println)
      }

      val customersWithLocationDataAndOptionalAbos =
        abos.rightOuterJoin(locations).collect
      highlight {
        println("Customers with location data and optional abos: ")
        customersWithLocationDataAndOptionalAbos.foreach(println)
      }

    })
  }

}
