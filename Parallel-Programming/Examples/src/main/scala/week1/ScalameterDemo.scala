package week1

import org.scalameter._

object ScalameterDemo {

  private def task() = (0 until 1000000).toArray
  private def printTime(time: Quantity[Double]) =
    s"Array initialization time: $time"

  def testMeasure(): String = printTime(measure{task()})

  def testWarmer(): String = printTime(withWarmer(new Warmer.Default).measure{task()})

  def testWarmerConfig(): String = printTime(config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.verbose -> true
  ).withWarmer(new Warmer.Default).measure{task()})

  def testMemory(): Quantity[Double] = withMeasurer(new Measurer.MemoryFootprint).measure{task()}
}
