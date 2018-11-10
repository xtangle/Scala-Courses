package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (ref, _) =>
      (ref, Signal(eval(getReferenceExpr(ref, namedExpressions), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalIter(expr: Expr, references: Map[String, Signal[Expr]], seen: Set[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) =>
          if (seen contains name) Double.NaN
          else evalIter(getReferenceExpr(name, references), references, seen + name)
        case Plus(a, b) => evalIter(a, references, seen) + evalIter(b, references, seen)
        case Minus(a, b) => evalIter(a, references, seen) - evalIter(b, references, seen)
        case Times(a, b) => evalIter(a, references, seen) * evalIter(b, references, seen)
        case Divide(a, b) =>
          val denom = evalIter(b, references, seen)
          if (denom == 0) Double.NaN
          else evalIter(a, references, seen) / denom
      }
    }
    evalIter(expr, references, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
