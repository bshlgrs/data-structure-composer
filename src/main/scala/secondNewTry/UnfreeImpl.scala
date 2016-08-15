package secondNewTry

import shared.BigOLiteral

/**
  * Created by buck on 7/31/16.
  */

case class UnfreeImpl(override val lhs: ImplLhs,
                      override val rhs: ImplRhs,
                      override val source: Option[ImplSource] = None) extends Impl(lhs, rhs, source) {
  // TODO: assert that all of the names are in the LHS
  assert(rhs.costs.keys.forall((k: MethodExpr) => true))

  // the minimum cost that this implementation could possibly have.
  def minCost: BigOLiteral = List(rhs.costs.values.min, rhs.constant).min

  def normalizedParameterCosts: List[BigOLiteral] = {
    // None of the RHS MethodExprs should have args.
    lhs.parameters.map((name) => rhs.costs(MethodExpr(name)))
  }

  def getDominance(other: UnfreeImpl): Dominance = {
    UnfreeImplDominance(this, other)
  }
}

object UnfreeImplDominance extends DominanceFunction[UnfreeImpl] {
  def apply(x: UnfreeImpl, y: UnfreeImpl): Dominance = {
    if (x.lhs.name != y.lhs.name) {
      Neither
    } else {
      val generalityDominance = y.lhs.dominance(x.lhs)
      val timeDominance = Dominance.fromSeqOfOrderedThings(
        y.normalizedParameterCosts.zip(x.normalizedParameterCosts))
      generalityDominance.infimum(timeDominance)
    }
  }
}
