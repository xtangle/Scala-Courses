package week3

case class CFFPurchase(customerId: Int, destination: String, price: Double)

object CFFPurchase {

  val purchases = List(
    CFFPurchase(100, "Geneva", 22.25),
    CFFPurchase(300, "Zurich", 42.10),
    CFFPurchase(100, "Fribourg", 12.40),
    CFFPurchase(200, "St. Gallen", 8.20),
    CFFPurchase(100, "Lucerne", 31.60),
    CFFPurchase(300, "Basel", 16.20))

}