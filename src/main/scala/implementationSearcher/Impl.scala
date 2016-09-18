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
case class Impl(lhs: ImplLhs, rhs: AffineBigOCombo[MethodExpr], source: Option[ImplSource] = None) {
  override def toString: String = {
    s"$lhs <- $rhs " + source.map("(from " + _ + ")").getOrElse("")
  }

  def bindToAllOptions(searchResult: SearchResult): Set[UnfreeImpl] = {
    unboundCosts.toList match {
      case Nil => Set(this.toUnfreeImpl.get)
      case (methodExpr, methodCostWeight) :: other => {
        val otherwiseSubbedImpls = this.copy(rhs = AffineBigOCombo(this.rhs.k, other.toMap ++ boundCosts)).bindToAllOptions(searchResult)

        val optionsAndConditions = searchResult.implsWhichMatchMethodExpr(methodExpr, lhs.implPredicateMap)

        val options: Set[UnfreeImpl] = searchResult.get(methodExpr.name)




        for {
          unfreeImpl <- otherwiseSubbedImpls
          (option, conditions, optionRhs) <- optionsAndConditions
        } yield {
          UnfreeImpl(
            lhs.addConditions(conditions),
            unfreeImpl.rhs + optionRhs * methodCostWeight,
            source)
        }
      }
    }

  }

  def unboundCosts: Map[MethodExpr, BigOLiteral] = {
    rhs.m.filterKeys((x) => !lhs.parameters.contains(x.name.name))
  }

  def boundCosts: Map[MethodExpr, BigOLiteral] = {
    rhs.m.filterKeys((x) => lhs.parameters.contains(x.name.name))
  }

  def toUnfreeImpl: Option[UnfreeImpl] = {
    if (unboundCosts.isEmpty)
      Some(UnfreeImpl(lhs, rhs.copy(m = rhs.m.map({ case ((m: MethodExpr, c: BigOLiteral)) => m.name -> c})), source))
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

  type Rhs = AffineBigOCombo[MethodExpr]

  def rhs(string: String): Rhs = {
    MainParser.implRhs.parse(string).get.value
  }
}
