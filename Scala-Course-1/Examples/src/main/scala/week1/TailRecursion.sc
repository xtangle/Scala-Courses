object exercise {

  def factorial(x: Int) = {

    def factorialTailRecursive(x: Int, acc: Int): Int =
      if (x == 0) acc else factorialTailRecursive(x - 1, acc * x)

    factorialTailRecursive(x, 1)
  }

  factorial(0)
  factorial(1)
  factorial(2)
  factorial(4)
  factorial(6)

}