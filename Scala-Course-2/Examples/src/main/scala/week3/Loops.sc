object exercise {

  // Exponentiation using a while loop
  def power(x: Double, exp: Int): Double = {
    var r = 1.0
    var i = exp
    while (i > 0) {
      r = r * x
      i = i - 1
    }
    r
  }

  // While loop implemented using a function
  // Note 1: Condition and command must be passed by name
  // Note 2: WHILE is tail recursive, so can operate with constant stack space
  def WHILE(condition: => Boolean)(command: => Unit): Unit =
    if (condition) {
      command
      WHILE(condition)(command)
    }
    else ()

  /*
  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else REPEAT(command)(condition)
  }
  */

  // Exercise: obtain way of using `REPEAT { command } UNTIL { condition }` syntax

  class RepeatableCommand(command: => Unit) {
    def UNTIL(condition: => Boolean): Unit = {
      command
      if (condition) ()
      else UNTIL(condition)
    }
  }

  def REPEAT(command: => Unit): RepeatableCommand = new RepeatableCommand(command)

  var i = 0
  REPEAT { print("*"); i += 1 } UNTIL { i == 5 }

  // Same thing, but using a do while loop
  i = 0
  do {
    print("*")
    i += 1
  } while (i < 5)

  // For-loop equivalent in Scala
  for (i <- 1 until 3) {
    print(i + " ")
  }

  for (i <- 1 until 3; j <- "abc") println(i + " " + j)
  // Translates to:
  (1 until 3) foreach (i => "abc" foreach (j => println(i + " " + j)))
}