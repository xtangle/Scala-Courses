package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c < 0 || c > r) 0
      else if (r == 0) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], level: Int): Boolean =
        if (level < 0) false
        else if (chars.isEmpty) level == 0
        else loop(chars.tail, level + (chars.head match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }))
      loop(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (coins.isEmpty) if (money == 0) 1 else 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}
