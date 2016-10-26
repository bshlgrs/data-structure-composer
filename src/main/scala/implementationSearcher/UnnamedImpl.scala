package implementationSearcher

import shared.{DominanceFrontier, BigOLiteral}

/**
  * Created by buck on 10/23/16.
  */
case class UnnamedImpl(predicates: ImplPredicateMap, cost: AffineBigOCombo[MethodExpr]) {
  def withName(name: MethodName): Impl = Impl(ImplLhs(name, predicates), cost)
}

object UnnamedImpl {
  implicit def fromImpl(impl: Impl): UnnamedImpl = UnnamedImpl(impl.lhs.conditions, impl.rhs)
}
