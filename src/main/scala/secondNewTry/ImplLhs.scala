package secondNewTry

/**
  * Created by buck on 7/25/16.
  */
case class ImplLhs(name: String, parameters: List[String] = Nil, conditions: Set[ImplPredicate] = Set()) {
  // x dominates y if x can be used in every situation where y can be used
  def dominance(other: ImplLhs): Dominance = {
    if (name == other.name) {
      other.normalizedConditionsList.zip(normalizedConditionsList).map({
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

  def normalizedConditionsList: List[Set[String]] = {
    parameters.zipWithIndex.map { case (parameterName, index) =>
      conditions.filter(_.parameterName == parameterName).map(_.property)
    }
  }

  def canImplement(methodExpr: MethodExpr): Boolean = {
    methodExpr.canBeImplementedBy(this)
  }
}

case class ImplPredicate(parameterName: String, property: String)
