import java.util.concurrent.ForkJoinTask

import common._

import scala.collection.mutable
import scala.collection.parallel.{Combiner, Task}

object SplittersCombinators {

  trait Iterator[T] {
    def hasNext: Boolean
    def next(): T
    def foldLeft[S](z: S)(f: (S, T) => S): S = {
      var result = z
      while (hasNext) result = f(result, next())
      result
    }
  }

  trait Splitter[T] {
    def threshold: Int
    def split: Seq[Splitter[T]]
    def remaining: Int
    def foldLeft[S](z: S)(f: (S, T) => S): S
    def fold(z: T)(f: (T, T) => T): T = {
      if (remaining < threshold) foldLeft(z)(f)
      else {
        val children: Seq[ForkJoinTask[T]] = split.map(child => task { child.fold(z)(f) })
        children.map(_.join()).foldLeft(z)(f)
      }
    }
  }

  trait Traversable[T] {
    def foreach(f: T => Unit): Unit
    def newBuilder: mutable.Builder[T, Traversable[T]]
    def filter(p: T => Boolean): Traversable[T] = {
      val b = newBuilder
      for (x <- this) if (p(x)) b += x
      b.result
    }
  }

}