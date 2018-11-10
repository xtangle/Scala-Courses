package week3

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero!")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(y, x)
  def numer: Int = x / g
  def denom: Int = y / g

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
}