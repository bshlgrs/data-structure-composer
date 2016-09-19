package implementationSearcher

import parsers.MainParser
import shared.{ConstantTime, BigOLiteral}

/**
  * Created by buck on 7/31/16.
  */


/// The RHS here is just an expression like n**2
// The LHS is a list of conditions that need to be used for this implementation to work
case class UnfreeImpl(lhs: ImplLhs,
                      rhs: AffineBigOCombo[MethodName],
                      source: Option[ImplSource] = None) {

  import UnfreeImpl._

  def canEqual(a: Any) = a.isInstanceOf[UnfreeImpl]

  override def equals(that: Any): Boolean =
    that match {
      case that: UnfreeImpl => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = UnfreeImpl.hashCode() ^ lhs.hashCode() ^ rhs.hashCode()

  assert(lhs.parameters.nonEmpty || rhs.weights.isEmpty)

  override def toString: String = {
    s"$lhs <- $rhs" + source.map("(from " + _ + ")").getOrElse("")
  }

  def cost: BigOLiteral = {
    rhs.bias
  }

  // Does this UnfreeImpl work with a given set of conditions?
  def compatibleWithConditions(conditions: ImplPredicateList): Boolean = {
    lhs.conditions.list.zip(conditions.list).forall({case ((thisConditions, thoseConditions)) =>
      thisConditions subsetOf thoseConditions})
  }

  def bindToContext(methodExpr: MethodExpr, implPredicateMap: ImplPredicateMap): Option[(ImplPredicateMap, AffineBigOCombo[MethodName])] = {
    assert(methodExpr.args.length == this.lhs.parameters.length)

    val conditionsAndRhses: List[Option[(ImplPredicateMap, Rhs)]] = methodExpr.args.zipWithIndex.map({case (f: FunctionExpr, idx) => f match {
      case AnonymousFunctionExpr(properties, fRhs) => {
        // If the anonymous function has the necessary properties, then add no conditions and continue
        if (lhs.conditions.list(idx).subsetOf(properties)) {
          val costOfParamInMethodExprNames = fRhs

          val relevantParamName = lhs.parameters(idx)
          val weightOfParam = rhs.weights(MethodName(relevantParamName))

          val cost = costOfParamInMethodExprNames * weightOfParam

          Some((ImplPredicateMap.empty, cost))
        } else
          None
      }
      case NamedFunctionExpr(name) => {
        val relevantParamName = lhs.parameters(idx)
        val weightOfParam = rhs.weights.getOrElse(MethodName(relevantParamName), {
          println(s"$this, $methodExpr")
          ???
        })

        Some(
          (
            ImplPredicateMap(Map(name -> lhs.conditions.list(idx))),
            AffineBigOCombo[MethodName](ConstantTime, Map(MethodName(name) -> weightOfParam))
            )
        )
      }
    }})

    if (conditionsAndRhses contains None)
      None
    else if (conditionsAndRhses.isEmpty) {
      Some(ImplPredicateMap.empty, this.rhs)
    } else {
      val (conditionsList, rhsList) = conditionsAndRhses.flatten.unzip

      Some(conditionsList.reduce(_.and(_)) -> (rhsList.reduce(_ + _) + rhs.bias))
    }
  }

  def alphaConvert(newParameterNames: List[String]): UnfreeImpl = {
    assert(newParameterNames.length == lhs.parameters.length)
    assert(newParameterNames.toSet.size == newParameterNames.size)

    UnfreeImpl(
      lhs.alphaConvert(newParameterNames),
      rhs.mapKeys((x: MethodName) => MethodName(newParameterNames(lhs.parameters.indexOf(x.name)))),
      source
    )
  }

}

object UnfreeImpl {
  def apply(string: String): UnfreeImpl = {
    MainParser.unfreeImpl.parse(string).get.value
  }

  type Rhs = AffineBigOCombo[MethodName]

  def rhs(string: String): Rhs = {
    MainParser.affineBigONameCombo.parse(string).get.value
  }
}

//object UnfreeImplDominance extends DominanceFunction[UnfreeImpl] {
//  def apply(x: UnfreeImpl, y: UnfreeImpl): Dominance = {
//    if (x.lhs.name != y.lhs.name) {
//      Neither
//    } else {
//      val generalityDominance = y.lhs.dominance(x.lhs)
//      val timeDominance = Dominance.fromSeqOfOrderedThings(
//        y.normalizedParameterCosts.zip(x.normalizedParameterCosts))
//      generalityDominance.infimum(timeDominance)
//    }
//  }
//}
