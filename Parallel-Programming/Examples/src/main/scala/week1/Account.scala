package week1

class Account(private var amount: Int = 0) {
  def transfer(target: Account, n: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }
}

object Account {
  def startThread(a: Account, b: Account, n: Int): Thread = {
    val t = new Thread {
      override def run(): Unit = {
        for (i <- 0 until n) {
          a.transfer(b, 1)
        }
      }
    }
    t.start()
    t
  }

  def main(args: Array[String]): Unit = {
    val a1 = new Account(500000)
    val a2 = new Account(700000)

    val t = startThread(a1, a2, 150000)
    val s = startThread(a2, a1, 150000)
    t.join()
    s.join()
    println(a1.amount)
    println(a2.amount)
  }
}
