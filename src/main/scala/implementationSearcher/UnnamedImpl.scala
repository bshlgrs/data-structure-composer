package implementationSearcher

import shared._
import shared.PartialOrdering

/**
  * Created by buck on 10/23/16.
  */
case class UnnamedImpl(predicates: ImplPredicateMap,
                       cost: AffineBigOCombo[MethodExpr],
                       source: ImplSource) {
  def withName(name: MethodName): Impl = Impl(ImplLhs(name, predicates), cost, source)

  // returns true if conditions imply this
  def compatibleWithConditions(conditions: ImplPredicateMap): Boolean = {
    implicitly[PartialOrdering[ImplPredicateMap]]
      .partialCompare(conditions, this.predicates)
      .leftDominates
  }
}

object UnnamedImpl {
  implicit def fromImpl(impl: Impl): UnnamedImpl = {
    UnnamedImpl(impl.lhs.conditions, impl.rhs, impl.source)
  }

  implicit object UnnamedImplPartialOrdering extends PartialOrdering[UnnamedImpl] {
    def partialCompare(x: UnnamedImpl, y: UnnamedImpl): DominanceRelationship = {
      val generalityDominance = implicitly[PartialOrdering[ImplPredicateMap]].partialCompare(x.predicates, y.predicates)
      val timeDominance = x.cost.partialCompare(y.cost)
      generalityDominance.infimum(timeDominance)
    }
  }
}
