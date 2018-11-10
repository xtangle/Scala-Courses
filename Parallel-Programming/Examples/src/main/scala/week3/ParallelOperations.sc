object ParallelOperations {

  def initializeArray(xs: Array[Int])(v: Int): Unit = {
    for (i <- xs.indices.par) {
      xs(i) = v
    }
  }

  val arr = new Array[Int](10)
  initializeArray(arr)(1)
  arr

  (1 until 1000).par
    .filter(n => n % 3 == 0)
    .count(n => n.toString == n.toString.reverse)

  def sum1(xs: Array[Int]): Int = {
    xs.par.foldLeft(0)(_ + _) // Not parallel!
  }

  def sum2(xs: Array[Int]): Int = {
    xs.par.fold(0)(_ + _) // Is parallel
  }

  sum1(Array(1, 2, 3, 4, 5))
  sum2(Array(1, 2, 3, 4, 5))

  def max(xs: Array[Int]): Int = {
    xs.par.fold(Int.MinValue)(math.max)
  }

  max(Array(1))
  max(Array(3, 2, 0, -6, 5, 1, 4, -2))

  /**
    * Rock paper scissors example
    **/
  def play(a: String, b: String): String = List(a, b).sorted match {
    case List("paper", "rock") => "paper"
    case List("paper", "scissors") => "scissors"
    case List("rock", "scissors") => "rock"
    case List(x, y) if x == y => x
    case List("", y) => y
  }

  // Result depends on execution schedule, why?
  // The play operator is commutative, but not associative
  Array("paper", "rock", "paper", "scissors")
    .par.fold("")(play)

  play(play("paper", "rock"), play("paper", "scissors"))

  play("paper", play("rock", play("paper", "scissors")))

  /**
    * Vowel counting example
    **/
  def isVowel(c: Char): Boolean =
    List('a', 'e', 'i', 'o', 'u').contains(c.toLower)

  Array('E', 'P', 'F', 'L', 'S', 'C', 'A', 'L', 'A').par.aggregate(0)(
    (count: Int, c: Char) => if (isVowel(c)) count + 1 else count,
      _ + _
  )

}