package week4

import java.util.NoSuchElementException

import scala.annotation.tailrec

sealed trait Conc[+T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]

  def <>[S >: T](that: Conc[S]): Conc[S] = {
    if (this == Empty) that
    else if (that == Empty) this
    else Conc.concat(this, that)
  }
}

object Conc {
  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) <>(xs, ys)
    else if (diff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = <>(xs.right.left, nrr)
          <>(nl, nr)
        } else {
          val nl = <>(xs.left, xs.right.left)
          val nr = nrr
          <>(nl, nr)
        }
      }
    } else {
      concat(ys, xs)
    }
  }
}

case object Empty extends Conc[Nothing] {
  def level = 0
  def size = 0
  def left = throw NoSuchElementException
  def right = throw NoSuchElementException
}

case class Single[T](x: T) extends Conc[T] {
  def level = 0
  def size = 1
  def left = Empty
  def right = Empty
}

case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level: Int = 1 + math.max(left.level, right.level)
  val size: Int = left.size + right.size
}

case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level: Int = 1 + math.max(left.level, right.level)
  val size: Int = left.size + right.size
}

object Append {
  def appendLeaf[T](xs: Conc[T], ys: Single[T]): Conc[T] = xs match {
    case Empty => ys
    case xs: Single[T] => <>(xs, ys)
    case _ <> _ => new Append(xs, ys)
    case xs: Append[T] => append(xs, ys)
  }

  def appendLeaf[T](xs: Conc[T], ys: Chunk[T]): Conc[T] = xs match {
    case Empty => ys
    case xs: Single[T] => <>(xs, ys)
    case _ <> _ => new Append(xs, ys)
    case xs: Append[T] => append(xs, ys)
  }

  @tailrec
  private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) new Append(xs, ys)
    else {
      val zs = <>(xs.right, ys)
      xs.left match {
        case ws @ Append(_, _) => append(ws, zs)
        case ws if ws.level <= zs.level => ws <> zs
        case ws => new Append(ws, zs)
      }
    }
  }
}

case class Chunk[T](array: Array[T], size: Int) extends Conc[T] {
  def level = 0
  def left = Empty
  def right = Empty
}