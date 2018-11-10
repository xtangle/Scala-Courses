package week4

import scala.reflect.ClassTag
import week4.Append._

class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
  private var chunk: Array[T] = new Array(k)
  private var chunkSize: Int = 0

  final def +=(elem: T): Unit = {
    if (chunkSize >= k) expand()
    chunk(chunkSize) = elem
    chunkSize += 1
  }

  final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    val combinedConc = this.result <> that.result
    new ConcBuffer(k, combinedConc)
  }

  def result: Conc[T] = {
    conc = appendLeaf(conc, Chunk(chunk, chunkSize))
    conc
  }

  private def expand(): Unit = {
    conc = appendLeaf(conc, Chunk(chunk, chunkSize))
    chunk = new Array(k)
    chunkSize = 0
  }

}

