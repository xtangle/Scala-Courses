package week1

object GetUniqueId {
  private var uidCount = 0L

  def getUniqueId: Long = {
    uidCount += 1
    uidCount
  }

  def startThread(): Thread = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (_ <- 0 until 10) yield getUniqueId
        println(uids)
      }
    }
    t.start()
    t
  }

  def main(args: Array[String]): Unit = {
    startThread()
    startThread()
  }
}