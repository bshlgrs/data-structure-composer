package secondNewTry

import shared.BigOLiteral

/**
  * Created by buck on 7/25/16.
  *
  * Something like
  *

countBetweenBy[f] <- unorderedEach[f]
getMaximum <- getFirstBy[valueOrdering]
getMinimum <- getLastBy[valueOrdering]
deleteMinimumBy![f] <- getMinimumBy[f] + deleteNode!

  */
class Impl(val lhs: ImplLhs, val rhs: ImplRhs, val source: Option[ImplSource] = None) {
  // takes a method like countBetweenBy[f] <- unorderedEach[f] and turns it into all the different
  // potentially cheapest options for its implementation.

  // There might be no available implementation, eg if unorderedEach has not been implemented.

  // There might be several, eg if reduce[f] can be implemented by either `if f.commutative <- unorderedEach[f]` or `each[f]`.
//  def chooseImplsAndBind(settledImpls: Set[UnfreeImpl]): Option[DominanceFrontier[UnfreeImpl]] = {
//    if (unboundCosts.keys.forall(_.canBeImplementedByAnyOf(settledImpls))) {
//      val allCostOptions = unboundCosts.keys.map((m: MethodExpr) => m.getFastestRelevantImplementationIfAny(settledImpls))
//
//
//    } else {
//      None
//    }
//  }


  // how do I pass implementation conditions into here? :/
  def bindToAllOptions(searchResult: SearchResult): Set[UnfreeImpl] = {
    unboundCosts.toList match {
      case Nil => Set(this.toUnfreeImpl.get)
      case (methodExpr, methodCostWeight) :: other => {
        val otherwiseSubbedImpls = Impl(lhs, rhs = ImplRhs(this.rhs.constant, other.toMap), source).bindToAllOptions(searchResult)

        val options = searchResult.implsWhichMatchMethodExpr(methodExpr)

        for {
          unfreeImpl <- otherwiseSubbedImpls
          option <- options
        } yield {
          UnfreeImpl(
            lhs.addConditions(option.lhs.conditions),
            unfreeImpl.cost + option.cost * methodCostWeight,
            source)
        }
      }
    }

  }

  def unboundCosts: Map[MethodExpr, BigOLiteral] = {
    rhs.costs.filterKeys((x) => !lhs.parameters.contains(x.name))
  }

  def toUnfreeImpl: Option[UnfreeImpl] = {
    if (rhs.costs.isEmpty)
      Some(UnfreeImpl(lhs, rhs.constant, source))
    else
      None
  }

  def addConditions(conditions: ImplPredicateList): Impl = {
    Impl(lhs.addConditions(conditions), rhs, source)
  }
}

object Impl {
  def apply(lhs: ImplLhs, rhs: ImplRhs, source: Option[ImplSource] = None): Impl = {
    new Impl(lhs, rhs, source)
  }
}
