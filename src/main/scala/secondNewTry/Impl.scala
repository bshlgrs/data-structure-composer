package secondNewTry

import shared.BigOLiteral

/**
  * Created by buck on 7/25/16.
  */
case class Impl(lhs: ImplLhs, rhs: ImplRhs, source: Option[ImplSource] = None) {
  def bind(settledImpls: Set[UnfreeImpl]): Option[DominanceFrontier[UnfreeImpl]] = {
    if (unboundCosts.keys.forall(_.canBeImplementedByAnyOf(settledImpls))) {
      val allCostOptions = unboundCosts.keys.map((m: MethodExpr) => m.bind(settledImpls))

      ???
    } else {
      None
    }
  }

  def unboundCosts: Map[MethodExpr, BigOLiteral] = {
    rhs.costs.filterKeys((x) => !lhs.parameters.contains(x.name))
  }
}
