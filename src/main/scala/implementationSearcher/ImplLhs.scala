package implementationSearcher

import implementationSearcher.ImplLhs.FunctionCondition

/**
  * Created by buck on 7/25/16.
  */
case class ImplLhs(name: MethodName, parameters: List[String], conditions: ImplPredicateList) {
  // x dominates y if x can be used in every situation where y can be used
  def dominance(other: ImplLhs): Dominance = {
    if (name == other.name) {
      other.conditions.list.zip(other.conditions.list).map({
        case (otherConditions: Set[String], thisConditions: Set[String]) =>
          Dominance.fromTwoBools(
            thisConditions.subsetOf(otherConditions),
            otherConditions.subsetOf(thisConditions)
          )
        }
      ).reduceOption(_ infimum _).getOrElse(Both)
    } else {
      Neither
    }
  }

  def canImplement(methodExpr: MethodExpr): Boolean = {
    methodExpr.canBeImplementedBy(this)
  }

  def addConditions(conditions: ImplPredicateList): ImplLhs = {
    this.copy(conditions = this.conditions.and(conditions))
  }
}

object ImplLhs {
  def apply(name: String, parameters: List[String] = Nil, conditions: ImplPredicateList = ImplPredicateList(Nil)): ImplLhs = {
    ImplLhs(MethodName(name), parameters, conditions)
  }

  type FunctionCondition = String
}

case class ImplPredicate(parameterIdx: Int, property: String)

case class ImplPredicateList(list: List[Set[FunctionCondition]]) {
  def and(other: ImplPredicateList): ImplPredicateList = {
    assert(this.list.length == other.list.length)

    ImplPredicateList(this.list.zip(other.list).map({ case ((x, y)) => x union y }))
  }
}
