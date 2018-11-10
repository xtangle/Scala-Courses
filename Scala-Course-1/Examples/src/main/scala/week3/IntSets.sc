object exercise {

  val t1: IntSet = new NonEmpty(3)
  val t2: IntSet = t1 incl 1
  val t3: IntSet = t2 incl 5

  val t4: IntSet = t1 incl 4
  val t5: IntSet = t3 union t4

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def this(elem: Int) = this(elem, Empty, Empty)
    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem
    override def toString = "{" + left + elem + right + "}"
  }

  object Empty extends IntSet {
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def contains(x: Int): Boolean = false
    def union(other: IntSet): IntSet = other
    override def toString = "."
  }

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

}