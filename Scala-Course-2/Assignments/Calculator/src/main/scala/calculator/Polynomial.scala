package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4.0*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case d if d > 0 =>
        val sqrtD = math.sqrt(d)
        Set((-b() + sqrtD) / (2.0*a()), (-b() - sqrtD) / (2.0*a()))
      case d if d == 0 => Set(-b() / (2.0*a()))
      case _ => Set()
    })
  }
}
