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

  assert(lhs.parameters.nonEmpty || rhs.m.isEmpty)

  override def toString: String = {
    s"$lhs <- $rhs" + source.map("(from " + _ + ")").getOrElse("")
  }

  def cost: BigOLiteral = {
    rhs.k
  }

  // Does this UnfreeImpl work with a given set of conditions?
  def compatibleWithConditions(conditions: ImplPredicateList): Boolean = {
    lhs.conditions.list.zip(conditions.list).forall({case ((thisConditions, thoseConditions)) =>
      thisConditions subsetOf thoseConditions})
  }

  // I'm using `otherMethod[g, h] if g.baz, h.fum` -- this is `this`
  // This is happening in the course of looking for unfreeImpls for an impl whose implPredicateMap is (f.foo, g.bar)
  // in my implementation, I used `otherMethod[f, _{fum}]` -- this is the methodExpr

  // I need to return `Some(f.foo, g.bar, f.baz)`.

  // If I was using `otherMethod[g, h] if h.baz`, then I'd return None, because that condition is incompatible with the usage.

  /*
  Simpler example for NamedFunctionExpr:

   this: `otherMethod[g] if g.baz`
   implPredicateMap: ()
   methodExpr: `otherMethod[f]`

   res: Some(Map(f -> baz))

   */
  def necessaryConditionsToMatch(methodExpr: MethodExpr, implPredicateMap: ImplPredicateMap): Set[ImplPredicateMap] = {
    val argConditions = methodExpr.args.zipWithIndex.map({case (f: FunctionExpr, idx) => f match {
      case AnonymousFunctionExpr(properties, _) => {
        // If the anonymous function has the necessary properties, then add no conditions and continue
        if (properties.subsetOf(lhs.conditions.list(idx)))
          Some(ImplPredicateMap(Map()))
        else
          None
      }
      case NamedFunctionExpr(name) => {
        Some(ImplPredicateMap(Map(name -> lhs.conditions.list(idx))))
      }
    }})

    if (argConditions contains None)
      Set()
    else {
      Set(argConditions.flatten.reduceOption(_.and(_)).getOrElse(ImplPredicateMap(Map())))
    }
  }

  def bindToContext(methodExpr: MethodExpr, implPredicateMap: ImplPredicateMap): Option[(ImplPredicateMap, AffineBigOCombo[MethodName])] = {
    val conditionsAndRhses: List[(ImplPredicateMap, Rhs)] = methodExpr.args.zipWithIndex.flatMap({case (f: FunctionExpr, idx) => f match {
      case AnonymousFunctionExpr(properties, fRhs) => {
        // If the anonymous function has the necessary properties, then add no conditions and continue
        if (properties.subsetOf(lhs.conditions.list(idx))) {
          val costOfParamInMethodExprNames = fRhs

          val relevantParamName = lhs.parameters(idx)
          val weightOfParam = rhs.m(MethodName(relevantParamName))

          val cost = costOfParamInMethodExprNames * weightOfParam

          Some((ImplPredicateMap.empty, cost))
        } else
          None
      }
      case NamedFunctionExpr(name) => {
        val relevantParamName = lhs.parameters(idx)

        Some(
          (
            ImplPredicateMap(Map(name -> lhs.conditions.list(idx))),
            AffineBigOCombo[MethodName](ConstantTime, Map(MethodName(relevantParamName) -> ConstantTime))
            )
        )
      }
    }})

    if (conditionsAndRhses contains None)
      None
    else if (conditionsAndRhses.isEmpty) {
      Some(ImplPredicateMap.empty, this.rhs)
    } else {
      val (conditionsList, rhsList) = conditionsAndRhses.unzip

      Some(conditionsList.reduce(_.and(_)) -> rhsList.reduce(_ + _))
    }
  }

}

object UnfreeImpl {
  def apply(string: String): UnfreeImpl = {
    MainParser.unfreeImpl.parse(string).get.value
  }

  type Rhs = AffineBigOCombo[MethodName]

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
