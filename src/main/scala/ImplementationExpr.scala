import java.awt.Container

/**
  * Created by buck on 5/7/16.
  */

case class ImplementationStmt(methodName: String,
                              arguments: List[String],
                              predicates: List[ImplementationPredicate],
                              implementation: ImplementationExpr) {
  assert(!methodName.contains(" "), "There was a space in your method name! :(")

  override def toString = {
    val argumentString = if (arguments.nonEmpty) s"[${arguments.mkString(",")}]" else ""
    val predicateString = if (predicates.nonEmpty) s" if ${predicates.map(_.toString).mkString(", ")}" else ""
    s"$methodName$argumentString$predicateString <- $implementation"
  }
}

case class MethodProperty(name: String) {
  override def toString = name
}

case class ImplementationPredicate(item: String, property: MethodProperty) {
  override def toString = s"$item.$property"
}

abstract class ImplementationExpr {
  override def toString = this match {
    case MethodUse(name, Nil) => name
    case MethodUse(name, args) => s"$name[${args.map(_.toString).mkString(",")}]"
    case Sum(args) => "(" + args.map(_.toString).mkString(" + ") + ")"
    case Product(args) => args.map(_.toString).mkString(" * ")
    case MinimumOf(args) => "(" + args.map(_.toString).mkString(" | ") + ")"
    case Constant(expr) => expr.toShortString
  }

  def +(other: ImplementationExpr) = (this, other) match {
    case (Constant(lhs), Constant(rhs)) => Constant(lhs + rhs)
    case _ => Sum(this.summands ++ other.summands)
  }

  def *(other: ImplementationExpr) = (this, other) match {
    case (Constant(lhs), Constant(rhs)) => Constant(lhs * rhs)
    case _ => Product(this.factors ++ other.factors)
  }

  def minWith(other: ImplementationExpr) = (this, other) match {
    case (Constant(lhs), Constant(rhs)) => Constant(lhs * rhs)
    case _ => MinimumOf(this.itemsBeingMinimumed ++ other.itemsBeingMinimumed)
  }

  private val summands: List[ImplementationExpr] = this match {
    case Sum(args) => args
    case _ => List(this)
  }

  private val factors: List[ImplementationExpr] = this match {
    case Product(args) => args
    case _ => List(this)
  }

  private val itemsBeingMinimumed = this match {
    case MinimumOf(args) => args
    case _ => List(this)
  }
}

case class MethodUse(name: String, args: List[FunctionExpr]) extends ImplementationExpr
case class Sum(args: List[ImplementationExpr]) extends ImplementationExpr
case class MinimumOf(args: List[ImplementationExpr]) extends ImplementationExpr
case class Product(args: List[ImplementationExpr]) extends ImplementationExpr
case class Constant(bigO: BigOExpression) extends ImplementationExpr

abstract class FunctionExpr {
  override def toString = this match {
    case MethodFunctionExpr(name, Nil) => name
    case MethodFunctionExpr(name, args) => s"$name[${args.map(_.toString).mkString(",")}]"
    case EmptyFunctionExpr => "_"
    case AnonymousFunctionExpr(properties, implementation) => {
      val propertyString = if (properties.nonEmpty) s"{${properties.mkString(", ")}}" else ""
      s"func$propertyString <- $implementation"
    }
  }
}

case class MethodFunctionExpr(name: String, args: List[FunctionExpr]) extends FunctionExpr
case object EmptyFunctionExpr extends FunctionExpr
case class AnonymousFunctionExpr(properties: List[MethodProperty],
                                 implementation: ImplementationExpr)
  extends FunctionExpr
