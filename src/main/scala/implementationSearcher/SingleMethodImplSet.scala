package implementationSearcher

import shared.{DominanceFrontier, BigOLiteral}

/**
  * Created by buck on 9/10/16.
  */
case class SingleMethodImplSet(options: DominanceFrontier[UnnamedImpl]) {
  def impls = options.items

  def bestImplementationForConditions(conditions: ImplPredicateMap): Option[Impl] = {
    implsWhichMatchConditions(conditions).toList.sortBy(_.rhs).headOption
  }

  def add(impl: UnnamedImpl): SingleMethodImplSet = {
    SingleMethodImplSet(options.add(impl))
  }

  def isOtherImplUseful(impl: UnnamedImpl): Boolean = {
    options.partialCompareToItem(impl).rightDominates
  }

  // More impl conditions means that this function returns something better
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, scope: UnfreeImplSet): Set[Impl] =
    options.items.flatMap({ (option: UnnamedImpl) =>
      option.bindToContext(methodExpr, scope)
    })

  // More impl conditions means that this function returns something better
  def implsWhichMatchConditions(implPredicates: ImplPredicateMap): Set[Impl] = {
    options.filter(_.compatibleWithConditions(implPredicates))
  }

  def toLongString: String = {
    s"{ " + impls.toList.map((impl) => {
      s"    (${impl.predicates.toNiceString}) <- ${impl.cost}"
    }).mkString("\n") + "  }"
  }

  def sum(other: SingleMethodImplSet): SingleMethodImplSet = {
    SingleMethodImplSet(this.options ++ other.options)
  }

  def product(other: SingleMethodImplSet): SingleMethodImplSet = {
    assert(this.name == other.name)

    SingleMethodImplSet.fromSet(for {
      x <- this.impls
      y <- other.impls
    } yield {
      val translatedY = y.alphaConvert(x.lhs.parameters)
      val lhs = x.lhs.addConditions(y.lhs.conditions)
      val rhs = x.rhs + y.rhs
      Impl(lhs, rhs, x.source)
    })
  }

  def bestFullyGeneralTime: Option[AffineBigOCombo[MethodName]] = {
    this.bestImplementationForConditions(ImplPredicateList.empty(impls.head.lhs.parameters.length)).map(_.rhs)
  }
}

object SingleMethodImplSet {
  def fromSet(set: Set[Impl]): SingleMethodImplSet = {
    SingleMethodImplSet(DominanceFrontier.fromSet(set))
  }
}
