import scala.util.Try

object exercise {

  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  Try(capitalOfCountry("US"))
  Try(capitalOfCountry("Andorra"))

  capitalOfCountry get "US"
  capitalOfCountry get "Andorra"

  def showCapital(country: String): String = capitalOfCountry get country match {
    case Some(capital) => capital
    case _ => "missing data"
  }

  showCapital("US")
  showCapital("Andorra")

  val cap1 = capitalOfCountry withDefaultValue "<unknown>"
  cap1("Andorra")

  val fruit = List("apple", "pear", "orange", "pineapple")

  fruit sortWith (_.length < _.length)
  fruit groupBy (_.head)

}