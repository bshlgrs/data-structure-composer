package firstNewTry

/**
  * Created by buck on 5/7/16.
  */



case class Impl(methodName: String,
                parameters: List[String],
                predicates: List[ImplPredicate],
                impl: ImplRhs) {

  lazy val implLhs = ImplLhs(methodName, parameters, predicates)

  assert(!methodName.contains(" "), "There was a space in your method name! :(")

  override def toString = {
    s"$implLhs <- $impl"
  }

  def isSuperSimple: Boolean = {
    parameters.isEmpty && predicates.isEmpty
  }

//  def timeOnOwn: Option[BigOExpression] = impl.minimumTimeOnOwn

//  def timeWith(otherCosts: Map[String, BigOExpression]): Option[BigOExpression] = {
//    val internalCosts = parameters.map(_ -> ConstantTime).toMap
//    impl.timeWithSubstitutions((use) => (internalCosts ++ otherCosts).get(use.name))
//  }
//
//  def timeWithArgs(otherCosts: Map[String, BigOExpression], args: List[BigOExpression]) = {
//    assert(args.length == parameters.length)
//  }

  def internalTime: Option[ImplRhs] = {
    if (isEntirelyUnfree)
      Some(impl)
    else
      None
  }

  def freeVariables: List[String] = impl.allMethodUses.map(_.name).filter(!parameters.contains(_))

  def isEntirelyUnfree: Boolean = freeVariables.isEmpty
}

case class MethodProperty(name: String) {
  override def toString = name
}

case class ImplPredicate(item: String, property: MethodProperty) {
  override def toString = s"$item.$property"
}

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
                                 implementation: ImplRhs)
  extends FunctionExpr
