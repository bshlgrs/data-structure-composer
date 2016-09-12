package secondNewTry

import shared.BigOLiteral

/**
  * Created by buck on 7/31/16.
  */


/// The RHS here is just an expression like n**2
// The LHS is a list of conditions that need to be used for this implementation to work
case class UnfreeImpl(override val lhs: ImplLhs,
                      cost: BigOLiteral,
                      override val source: Option[ImplSource] = None) extends Impl(lhs, ImplRhs(cost), source) {

  // Does this UnfreeImpl work with a given set of conditions?
  def compatibleWithConditions(conditions: ImplPredicateList): Boolean = {
    lhs.conditions.list.zip(conditions.list).forall({case ((thisConditions, thoseConditions)) =>
      thisConditions subsetOf thoseConditions})
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
