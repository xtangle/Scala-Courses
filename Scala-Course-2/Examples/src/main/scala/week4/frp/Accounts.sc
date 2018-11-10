import week4.frp.BankAccount
import week4.frp.{Signal, Var}

object exercise {

  def consolidated(accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  val a = new BankAccount()
  val b = new BankAccount()
  val c = consolidated(List(a, b))

  c()
  a deposit 20
  c()
  b deposit 30
  c()

  val xchange = Signal(246.00)
  val inDollar = Signal(c() * xchange())

  inDollar()
  b withdraw 10
  inDollar()

  // Exercise - do they yield the same final value for twice()? (Ans: No)
  val num_1 = Var(1)
  val twice_1 = Signal(num_1() * 2)
  num_1() = 2
  // twice_1() == 4

  var num_2 = Var(1)
  val twice_2 = Signal(num_2() * 2)
  num_2 = Var(2)
  // twice_2() == 2
}