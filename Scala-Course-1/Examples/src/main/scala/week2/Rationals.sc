object exercise {

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x + y
  -x
  x - y - z
  y + y
  x < y
  x max y

  new Rational(7)

  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be nonzero!")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(y, x)
    def numer: Int = x / g
    def denom: Int = y / g
    /*
    def numer: Int = x
    def denom: Int = y
    */

    def unary_- : Rational = new Rational(-numer, denom)

    def + (that: Rational): Rational =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

    def - (that: Rational): Rational = this + -that

    def < (that: Rational): Boolean = numer * that.denom < that.numer * denom

    def max(that: Rational): Rational = if (this < that) that else this

    override def toString: String = numer + "/" + denom
    /*
    override def toString: String = {
      val g = gcd(numer, denom)
      numer / g + "/" + denom / g
    }
    */
  }

}