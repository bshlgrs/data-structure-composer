package implementationSearcher

import parsers.MainParser
import shared._

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

  // I feel dirty for writing this
  override def hashCode: Int = UnfreeImpl.hashCode() ^ lhs.hashCode() ^ rhs.hashCode()

  assert(lhs.parameters.nonEmpty || rhs.weights.isEmpty, {
    s"Error in $this: LHS parameters are ${lhs.parameters}, but weights are ${rhs.weights}"
  })

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

  def bindToContext(methodExpr: MethodExpr, scope: Scope): Set[(ImplPredicateMap, AffineBigOCombo[MethodName])] = {
    assert(methodExpr.args.length == this.lhs.parameters.length,
    s"Assertion failed: bindToContext called on $this with methodExpr $methodExpr.\n" +
      s"These have a different number of args: ${this.lhs.parameters} vs ${methodExpr.args}.")

    val conditionsAndRhses: List[Set[(ImplPredicateMap, Rhs)]] = methodExpr.args.zipWithIndex.map({case (f: FunctionExpr, idx) =>
      val that = this
      val relevantParamName = lhs.parameters(idx)
      rhs.weights.get(MethodName(relevantParamName)) match {
        case None => Set((ImplPredicateMap.empty, AffineBigOCombo[MethodName](ConstantTime, Map())))
        case Some(weightOfParam) =>
          f.getConditionsAndCosts(lhs.conditions.list(idx), scope).map((x) => (x._1, x._2 * weightOfParam))
      }
    })

    val combinationsOfImpls: Set[List[(ImplPredicateMap, Rhs)]] = Utils.cartesianProducts(conditionsAndRhses)

    for {
      conditionsAndRhses <- combinationsOfImpls
    } yield {
      val (conditionsList, rhsList) = conditionsAndRhses.unzip
      val finalConditionsMap = conditionsList.reduceOption(_.and(_)).getOrElse(ImplPredicateMap.empty)
      val finalRhs = rhsList.reduceOption(_ + _).getOrElse(rhs) + rhs.bias
      finalConditionsMap -> finalRhs
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

  def toImpl: Impl = Impl(lhs, rhs.mapKeys((x) => MethodExpr(x, Nil)))
}

object UnfreeImpl {
  def apply(string: String): UnfreeImpl = {
    MainParser.nakedUnfreeImpl.parse(string).get.value
  }

  type Rhs = AffineBigOCombo[MethodName]

  def rhs(string: String): Rhs = {
    MainParser.nakedAffineBigONameCombo.parse(string).get.value
  }

  implicit object UnfreeImplPartialOrdering extends PartialOrdering[UnfreeImpl] {
    def partialCompare(x: UnfreeImpl, y: UnfreeImpl): DominanceRelationship = {
      if (x.lhs.name != y.lhs.name) {
        NeitherDominates
      } else {
        val alphaConvertedY = y.alphaConvert(x.lhs.parameters)
        val generalityDominance = implicitly[PartialOrdering[ImplLhs]].partialCompare(x.lhs, y.lhs)
        val timeDominance = x.rhs.partialCompare(alphaConvertedY.rhs)
        generalityDominance.infimum(timeDominance.flip)
      }
    }
  }
}

//object UnfreeImplDominance extends PartialOrdering[UnfreeImpl] {
//  def apply(x: UnfreeImpl, y: UnfreeImpl): Dominance = {
//    if (x.lhs.name != y.lhs.name) {
//      Neither
//    } else {
//      val generalityDominance = y.lhs.dominance(x.lhs)
//      val timeDominance = Dominance.fromSeqOfOrderedThings(
//      y.normalizedParameterCosts.zip(x.normalizedParameterCosts))
//      generalityDominance.infimum(timeDominance)
//    }
//  }
//}

