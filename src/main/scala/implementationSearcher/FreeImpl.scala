package implementationSearcher

import implementationSearcher.ImplLibrary.Decls
import shared.DominanceFrontier
import com.softwaremill.quicklens._

/**
  * Created by buck on 11/6/16.
  */
case class FreeImplSource(ds: Option[String]) {

}

case class FreeImpl(impl: Impl, freeImplSource: FreeImplSource) {
  def makeBound(unfreeImplSet: UnfreeImplSet, decls: Decls) = {
    assert(impl.unboundCostTuples(unfreeImplSet, decls).isEmpty)

    BoundImpl(impl, BoundSource(this, Set()))
  }

  lazy val implString = impl.toString

  // Suppose you have an implementation, like

  // f[x] <- g[x]

  // and you have implementations

  // g[y] <- y * n
  // g[y] if y.foo <- y * log(n)

  // This method returns

  // f[x] <- x * n
  // f[x] if x.foo <- x * log(n)
  def bindToAllOptions(unfreeImplSet: UnfreeImplSet, decls: ImplLibrary.Decls): DominanceFrontier[BoundUnnamedImpl] =
    impl.unboundCostTuples(unfreeImplSet, decls) match {
      case Nil => DominanceFrontier.fromSet(
        Set(this.modify(_.impl.rhs)
                .setTo(impl.boundCost(unfreeImplSet, decls))
                .makeBound(unfreeImplSet, decls))
        )
      case (methodExpr, methodCostWeight) :: other => {

        val otherwiseSubbedImpls = this
          .modify(_.impl.rhs)
          .using(_.filterKeys(_ != methodExpr))
          .bindToAllOptions(unfreeImplSet, decls)

        val optionsAndConditions =
          unfreeImplSet.implsWhichMatchMethodExpr(methodExpr,
            ParameterList(impl.lhs.conditions, decls(impl.name).parameters),
            decls)

        DominanceFrontier.fromSet(for {
          unfreeImpl <- otherwiseSubbedImpls.items
          optionImpl <- optionsAndConditions
        } yield {
          val unfreeSource = unfreeImpl.boundSource.materialSet
          val source = BoundSource(impl, unfreeSource + optionImpl.impl.withName(methodExpr.name))

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
