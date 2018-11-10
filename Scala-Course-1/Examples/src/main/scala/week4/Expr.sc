import week4._

object exercise {

  def show(e: Expr): String = {
    def checkForBrackets(e: Expr): String = e match {
      case Sum(e1, e2) => s"(${show(Sum(e1, e2))})"
      case expr => show(expr)
    }
    e match {
      case Number(n) => n.toString
      case Sum(e1, e2) => show(e1) + " + " + show(e2)
      case Prod(e1, e2) => checkForBrackets(e1) + " * " + checkForBrackets(e2)
      case Var(s) => s
    }
  }

  show(Sum(Number(1), Number(2)))
  show(Prod(Number(1), Number(2)))
  show(Sum(Prod(Number(2), Var("x")), Var("y")))
  show(Prod(Sum(Number(2), Var("x")), Var("y")))
  show(Prod(Prod(Sum(Number(2), Var("w")), Var("x")), Sum(Var("y"), Var("z"))))
}