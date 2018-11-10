package week4

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
  override def toString: String = s"($head$tail)"
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString: String = ""
}

object List {
  // def apply[T](): List[T] = new Nil[T]
  // def apply[T](x1: T): List[T] = new Cons[T](x1, this())
  // def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, this(x2))

  def apply[T](x: T*): List[T] = if (x.isEmpty) Nil else new Cons[T](x.head, this(x.tail: _*))
}
