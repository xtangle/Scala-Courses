object exercise {

  class Poly(val terms0: Map[Int, Double]) {

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms: Map[Int, Double] = terms0 withDefaultValue 0.0

/*
    def + (other: Poly): Poly = new Poly(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
/*    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
      }*/
      exp -> (coeff + terms(exp))
    }
*/

    /** This version using foldLeft is more efficient than adjust, because
      * we avoid creating a list of temporary terms
     */
    def + (other: Poly): Poly =
      new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (terms(exp) + coeff))
    }

    override def toString: String = {
      def formatCoeff(coeff: Double): String = coeff.toString.replaceAll(".0+$", "")
      (for ((exp, coeff) <- terms.toList.sorted.reverse.map(p => p._1 -> formatCoeff(p._2)))
        yield (exp, coeff) match {
          case (exp1, "0") => None
          case (0, coeff1) => Some(coeff1.toString)
          case (1, coeff1) => Some(s"${coeff1}x")
          case (exp1, coeff1) => Some(s"${coeff1}x^$exp1")
        }).filter(_.isDefined).map(_.get).mkString(" + ") match {
        case s if s.isEmpty => "0"
        case s => s
      }
    }
  }

  val p1 = new Poly(5 -> 6.2, 1 -> 2.0, 3 -> 4.0)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0, 5 -> 0.0)
  val p3 = new Poly()

  p1 + p2 + p3

}