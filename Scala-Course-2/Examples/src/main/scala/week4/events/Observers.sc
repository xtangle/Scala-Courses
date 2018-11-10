import week4.events.{BankAccount, Consolidator}

object exercise {
  val a = new BankAccount
  val b = new BankAccount
  val c = new Consolidator(List(a, b))

  c.totalBalance
  a deposit 20
  c.totalBalance
  b deposit 30
  c.totalBalance
}