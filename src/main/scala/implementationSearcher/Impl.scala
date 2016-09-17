package implementationSearcher

import parsers.MainParser
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
case class Impl(lhs: ImplLhs, rhs: ImplRhs, source: Option[ImplSource] = None) {
  override def toString: String = {
    s"$lhs <- $rhs " + source.map("(from " + _ + ")").getOrElse("")
  }

  // how do I pass implementation conditions into here? :/
  def bindToAllOptions(searchResult: SearchResult): Set[UnfreeImpl] = {
    unboundCosts.toList match {
      case Nil => Set(this.toUnfreeImpl.get)
      case (methodExpr, methodCostWeight) :: other => {
        val otherwiseSubbedImpls = this.copy(rhs = ImplRhs(this.rhs.constant, other.toMap ++ boundCosts)).bindToAllOptions(searchResult)

        val optionsAndConditions = searchResult.implsWhichMatchMethodExpr(methodExpr, lhs.implPredicateMap)

        for {
          unfreeImpl <- otherwiseSubbedImpls
          (option, conditions) <- optionsAndConditions
        } yield {
          UnfreeImpl(
            lhs.addConditions(conditions),
            unfreeImpl.rhs + option.rhs * methodCostWeight,
            source)
        }
      }
    }

  }

  def unboundCosts: Map[MethodExpr, BigOLiteral] = {
    rhs.costs.filterKeys((x) => !lhs.parameters.contains(x.name.name))
  }

  def boundCosts: Map[MethodExpr, BigOLiteral] = {
    rhs.costs.filterKeys((x) => lhs.parameters.contains(x.name.name))
  }

  def toUnfreeImpl: Option[UnfreeImpl] = {
    if (unboundCosts.isEmpty)
      Some(UnfreeImpl(lhs, rhs, source))
    else
      None
  }

  def addConditions(conditions: ImplPredicateList): Impl = {
    Impl(lhs.addConditions(conditions), rhs, source)
  }
}

object Impl {
  def apply(string: String): Impl = {
    MainParser.impl.parse(string).get.value
  }
}
