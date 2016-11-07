package implementationSearcher

import shared.DominanceFrontier

/**
  * Created by buck on 11/6/16.
  */
case class FreeImplSource(ds: Option[String]) {

}

case class FreeImpl(impl: Impl, freeImplSource: FreeImplSource) {
  def makeBound(unfreeImplSet: UnfreeImplSet) = {
    assert(impl.unboundCostTuples(unfreeImplSet).isEmpty)

    BoundImpl(impl, BoundSource.build(SingleBoundSource(this, Set())))
  }

  // Suppose you have an implementation, like

  // f[x] <- g[x]

  // and you have implementations

  // g[y] <- y * n
  // g[y] if y.foo <- y * log(n)

  // This method returns

  // f[x] <- x * n
  // f[x] if x.foo <- x * log(n)
  def bindToAllOptions(searchResult: UnfreeImplSet): DominanceFrontier[BoundUnnamedImpl] =
    impl.unboundCostTuples(searchResult) match {
      case Nil => DominanceFrontier.fromSet(
        Set(this.copy(impl = impl.copy(rhs = impl.boundCost(searchResult))).makeBound(searchResult)))
      case (methodExpr, methodCostWeight) :: other => {
        val otherwiseSubbedImpls = this.copy(impl=impl.copy(rhs = impl.rhs.filterKeys(_ != methodExpr))).bindToAllOptions(searchResult)

        val optionsAndConditions =
          searchResult.implsWhichMatchMethodExpr(methodExpr,
            ParameterList(impl.lhs.conditions, searchResult.declarations(impl.lhs.name).parameters))

        DominanceFrontier.fromSet(for {
          unfreeImpl <- otherwiseSubbedImpls.items
          optionImpl <- optionsAndConditions
        } yield {
          assert(unfreeImpl.boundSource.boundSources.size == 1)
          assert(optionImpl.boundSource.boundSources.size == 1)
          val unfreeSource = unfreeImpl.boundSource.boundSources.head.materials
          val source = BoundSource.build(SingleBoundSource(impl, unfreeSource + optionImpl.impl.withName(methodExpr.name)))

          BoundUnnamedImpl(
            UnnamedImpl(
              impl.lhs.conditions.and(optionImpl.impl.predicates),
              unfreeImpl.impl.cost + optionImpl.impl.cost * methodCostWeight),
            source
          )
        })
      }
    }
}

object FreeImpl {
  implicit def toImpl(freeImpl: FreeImpl): Impl = freeImpl.impl

  def parse(string: String) = FreeImpl(Impl(string), FreeImplSource(None))

  def wrap(impl: Impl): FreeImpl = FreeImpl(impl, FreeImplSource(None))
}
