package week1

class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello ")
    println("world!")
  }
}

object HelloThread {
  def main(args: Array[String]): Unit = {
    val s = new HelloThread
    val t = new HelloThread
    t.start()
    s.start()
    t.join()
    s.join()
  }
}
