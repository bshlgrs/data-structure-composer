package firstNewTry


abstract class ImplRhs {
  override def toString = this match {
    case MethodUse(name, Nil) => name
    case MethodUse(name, args) => s"$name[${args.map(_.toString).mkString(",")}]"
    case Sum(args) => "(" + args.map(_.toString).mkString(" + ") + ")"
    case Product(args) => args.map(_.toString).mkString(" * ")
    case MinimumOf(args) => "(" + args.map(_.toString).mkString(" | ") + ")"
    case Constant(expr) => expr.toString
  }

  def timeWithSubstitutions(substitute: MethodUse => Option[BigOExpression]): Option[BigOExpression] = this match {
    case mu: MethodUse => substitute(mu)
    case Sum(args) =>
      val subs = args.map(_.timeWithSubstitutions(substitute))
      subs.reduce((xOpt: Option[BigOExpression], yOpt: Option[BigOExpression]) => (xOpt, yOpt) match {
        case (Some(x), Some(y)) => Some(x + y)
        case _ => None
      })
    case Product(args) =>
      val subs = args.map(_.timeWithSubstitutions(substitute))
      subs.reduce((xOpt: Option[BigOExpression], yOpt: Option[BigOExpression]) => (xOpt, yOpt) match {
        case (Some(x), Some(y)) => Some(x * y)
        case _ => None
      })
    case MinimumOf(args) =>
      args.map(_.timeWithSubstitutions(substitute)).flatMap(_.toList) match {
        case Nil => None
        case list => Some(list.reduce(_ or _))
      }
    case Constant(x) => Some(x)
  }

  // if no other methods are defined, how long will this take
  lazy val minimumTimeOnOwn: Option[BigOExpression] = {
    timeWithSubstitutions((x) => None)
  }

  def allMethodUses: List[MethodUse] = this match {
    case MethodUse(name, Nil) => List(MethodUse(name, Nil))
    case MethodUse(name, args) => List(MethodUse(name, args))
    case Sum(args) => args.flatMap(_.allMethodUses)
    case Product(args) => args.flatMap(_.allMethodUses)
    case MinimumOf(args) => args.flatMap(_.allMethodUses)
    case Constant(expr) => Nil
  }

  def +(other: ImplRhs) = (this, other) match {
    case (Constant(lhs), Constant(rhs)) => Constant(lhs + rhs)
    case _ => Sum(this.summands ++ other.summands)
  }

  def *(other: ImplRhs) = (this, other) match {
    case (Constant(lhs), Constant(rhs)) => Constant(lhs * rhs)
    case _ => Product(this.factors ++ other.factors)
  }

  def minWith(other: ImplRhs) = (this, other) match {
    case (Constant(lhs), Constant(rhs)) => Constant(lhs * rhs)
    case _ => MinimumOf(this.itemsBeingMinimumed ++ other.itemsBeingMinimumed)
  }

  private val summands: List[ImplRhs] = this match {
    case Sum(args) => args
    case _ => List(this)
  }

  private val factors: List[ImplRhs] = this match {
    case Product(args) => args
    case _ => List(this)
  }

  private val itemsBeingMinimumed = this match {
    case MinimumOf(args) => args
    case _ => List(this)
  }
}

case class MethodUse(name: String, args: List[FunctionExpr]) extends ImplRhs {

}

case class Sum(args: List[ImplRhs]) extends ImplRhs
case class MinimumOf(args: List[ImplRhs]) extends ImplRhs
case class Product(args: List[ImplRhs]) extends ImplRhs
case class Constant(bigO: BigOExpression) extends ImplRhs
