object exercise {

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }

    loop(a, 1)
  }

  product(x => 2 * x)(1, 3)

  def factorial(x: Int): Int =
    if (x == 0) 1 else product(x => x)(1, x)

  factorial(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, combine(acc, f(a)))
    }

    loop(a, zero)
  }

  mapReduce(x => 2 * x, (x, y) => x * y, 1)(1, 3)

}