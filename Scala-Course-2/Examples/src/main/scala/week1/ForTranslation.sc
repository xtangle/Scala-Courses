object exercise {

  case class Book(title: String, authors: List[String])

  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  def isPrime(n: Int): Boolean =
    if (n <= 1) false
    else (2 to math.sqrt(n).toInt) forall (n % _ != 0)

  val n = 5

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  (1 until n) flatMap(i =>
    (1 until i).withFilter(j => isPrime(i + j))
      .map(j => (i, j)))

  for (b <- books; a <- b.authors if a startsWith "Bird")
    yield b.title

  books.flatMap(b =>
    b.authors.withFilter(_.startsWith("Bird"))
      .map(a => b.title))
}