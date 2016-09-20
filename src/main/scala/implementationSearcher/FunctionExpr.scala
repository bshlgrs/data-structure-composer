package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import shared.ConstantTime

/**
  * Created by buck on 7/31/16.
  */

abstract class FunctionExpr {
  def properties(conditions: Map[String, Set[FunctionProperty]]): Set[FunctionProperty] = this match {
    case NamedFunctionExpr(name) => conditions(name)
    case AnonymousFunctionExpr(defaultProperties, _) => defaultProperties
  }

  def getNames: Set[String] = this match {
    case NamedFunctionExpr(name) => Set(name)
    case AnonymousFunctionExpr(props, args) => args.weights.keys.map(_.name).toSet
  }
}

object UnderscoreFunctionExpr extends AnonymousFunctionExpr(Set(), AffineBigOCombo(ConstantTime, Map()))
case class NamedFunctionExpr(name: String) extends FunctionExpr {
  override def toString = name
}
// todo: check that time gets propagated correctly
case class AnonymousFunctionExpr(properties: Set[String], cost: AffineBigOCombo[MethodName] = AffineBigOCombo(ConstantTime, Map())) extends FunctionExpr {
  override def toString = s"_{${properties.mkString(",")}} <- $cost"
}
