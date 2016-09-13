package implementationSearcher

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
  override def toString: String = {
    s"$lhs <- $rhs " + source.map("(from " + _ + ")").getOrElse("")
  }

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
