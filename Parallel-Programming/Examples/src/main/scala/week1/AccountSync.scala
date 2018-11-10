package week1

class AccountSync(private var amount: Int = 0) {
  val uid: Long = GetUniqueIdSync.getUniqueId
  def lockAndTransfer(target: AccountSync, n: Int): Unit =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  def transfer(target: AccountSync, n: Int): Unit =
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
}

object AccountSync {
  def startThread(a: AccountSync, b: AccountSync, n: Int): Thread = {
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
    val a1 = new AccountSync(500000)
    val a2 = new AccountSync(700000)

    val t = startThread(a1, a2, 150000)
    val s = startThread(a2, a1, 150000)
    t.join()
    s.join()
    println(a1.amount)
    println(a2.amount)
  }
}
