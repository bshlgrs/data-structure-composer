package firstNewTry

/**
  * Created by buck on 7/21/16.
  */
sealed abstract class BigOExpression {
  def +(other: BigOExpression): BigOExpression = SumBigO(this, other)
  def *(other: BigOExpression): BigOExpression = ProductBigO(this, other)
  def or(other: BigOExpression): BigOExpression = OrBigO(this, other)

  def evaluate(vars: String => BigOLiteral): BigOLiteral = this match {
    case SumBigO(lhs, rhs) => lhs.evaluate(vars) + rhs.evaluate(vars)
    case ProductBigO(lhs, rhs) => lhs.evaluate(vars) * rhs.evaluate(vars)
    case OrBigO(lhs, rhs) => lhs.evaluate(vars) or rhs.evaluate(vars)
    case VariableBigOExpression(name) => vars(name)
  }

  def vars: Set[String] = this match {
    case SumBigO(lhs, rhs) => lhs.vars ++ rhs.vars
    case ProductBigO(lhs, rhs) => lhs.vars ++ rhs.vars
    case OrBigO(lhs, rhs) => lhs.vars ++ rhs.vars
    case VariableBigOExpression(name) => Set(name)
  }

  def doesStrictlyDominate(other: BigOExpression): Boolean = {
    assert(this.vars == other.vars)



    false
  }
}

case class SumBigO(lhs: BigOExpression, rhs: BigOExpression) extends BigOExpression
case class ProductBigO(lhs: BigOExpression, rhs: BigOExpression) extends BigOExpression
case class OrBigO(lhs: BigOExpression, rhs: BigOExpression) extends BigOExpression
case class LiteralBigO(lit: BigOLiteral) extends BigOExpression
case class VariableBigOExpression(name: String) extends BigOExpression
