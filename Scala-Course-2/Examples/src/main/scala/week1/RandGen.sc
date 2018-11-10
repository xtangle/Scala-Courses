import week1.Generator

object exercise {

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate: Int = rand.nextInt()
  }

  val booleans: Generator[Boolean] = for (x <- integers) yield x > 0
  // val booleans: Generator[Boolean] = integers map (_ > 0)

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] =
    t flatMap (x => u map (y => (x, y)))

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  // Lists Example

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists: Generator[List[Int]] = single(Nil)

  def nonEmptyLists: Generator[List[Int]] = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  lists.generate

  // Tree Generator exercise

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  def leafs: Generator[Tree] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Tree] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  trees.generate

  // Random Test Function

  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), s"test failed for $value")
    }
    println(s"passed $numTimes tests")
  }

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }
}