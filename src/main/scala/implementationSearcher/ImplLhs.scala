package implementationSearcher

import implementationSearcher.ImplLhs.FunctionCondition

/**
  * Created by buck on 7/25/16.
  */
case class ImplLhs(name: MethodName, parameters: List[String], conditions: ImplPredicateList) {
  assert(parameters.length == conditions.list.length, s"Impl for ${name.name} failed :/")

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

  override def toString: String = {
    val parametersString = parameters match {
      case Nil => ""
      case _ => s"[${parameters.mkString(", ")}]"
    }

    val conditionsString = if (conditions.isEmpty) "" else s"if ${conditions.toNiceString(parameters)}"

    s"${name.name}$parametersString $conditionsString"
  }
}

object ImplLhs {
  def apply(name: String,
            parameters: List[String] = Nil,
            conditions: Option[ImplPredicateList] = None): ImplLhs = {
    ImplLhs(MethodName(name), parameters, conditions.getOrElse(ImplPredicateList(parameters.map((_) => Set[FunctionCondition]()))))
  }

  type FunctionCondition = String
}

case class ImplPredicate(parameterIdx: Int, property: String)

case class ImplPredicateList(list: List[Set[FunctionCondition]]) {
  def and(other: ImplPredicateList): ImplPredicateList = {
    assert(this.list.length == other.list.length)

    ImplPredicateList(this.list.zip(other.list).map({ case ((x, y)) => x union y }))
  }

  def toNiceString(names: List[String]): String = {
    assert(names.length == list.length)

    list.zip(names).flatMap({ case ((set, name)) => set.toList.map(name + "." + _)}).mkString(", ")
  }

  def isEmpty: Boolean = list.forall(_.isEmpty)
}
