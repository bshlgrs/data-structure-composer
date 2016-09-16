package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty

/**
  * Created by buck on 7/31/16.
  */

abstract class FunctionExpr {
  def properties(conditions: Map[String, Set[FunctionProperty]]): Set[FunctionProperty] = this match {
    case NamedFunctionExpr(name) => conditions(name)
    case AnonymousFunctionExpr(defaultProperties) => defaultProperties
  }
}

object UnderscoreFunctionExpr extends AnonymousFunctionExpr(Set())
case class NamedFunctionExpr(name: String) extends FunctionExpr
case class AnonymousFunctionExpr(properties: Set[String]) extends FunctionExpr
