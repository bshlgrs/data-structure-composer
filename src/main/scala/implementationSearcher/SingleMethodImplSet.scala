package implementationSearcher

import shared._

/**
  * Created by buck on 9/10/16.
  */
case class SingleMethodImplSet(options: DominanceFrontier[UnnamedImpl], map: Map[UnnamedImpl, ImplSource]) {
  def impls = options.items

  def bestImplsForConditions(conditions: ImplPredicateMap): DominanceFrontier[UnnamedImpl] = {
    DominanceFrontier.fromSet(implsWhichMatchConditions(conditions).items)
  }

  def add(impl: UnnamedImpl, source: ImplSource): SingleMethodImplSet = {
    SingleMethodImplSet(options.add(impl), map.updated(impl, source))
  }

  def isOtherImplUseful(impl: UnnamedImpl): Boolean = {
    List(RightStrictlyDominates, NeitherDominates).contains(options.partialCompareToItem(impl))
  }

  // More impl conditions means that this function returns something better
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, scope: UnfreeImplSet, list: ParameterList): Set[UnnamedImpl] =
    options.items.flatMap({ (option: UnnamedImpl) =>
      option.withName(methodExpr.name).bindToContext(methodExpr, scope, list)
    })

  // More impl conditions means that this function returns something better
  def implsWhichMatchConditions(implPredicates: ImplPredicateMap): DominanceFrontier[UnnamedImpl] = {
    options.filter(_.compatibleWithConditions(implPredicates))
  }

  def toLongString: String = {
    s"{ " + impls.toList.map((impl) => {
      s"    (${impl.predicates.toNiceString}) <- ${impl.cost}"
    }).mkString("\n") + "  }"
  }

  def sum(other: SingleMethodImplSet): SingleMethodImplSet = {
    SingleMethodImplSet(this.options ++ other.options, this.map ++ other.map)
  }

  def product(other: SingleMethodImplSet): SingleMethodImplSet =
    SingleMethodImplSet.fromSetOfTuples(for {
      x <- this.impls
      y <- other.impls
    } yield {
      val lhs = x.predicates.and(y.predicates)
      val rhs = x.cost + y.cost
      UnnamedImpl(lhs, rhs) -> ProductSource(other.map(x), other.map(y))
    })


//  def bestFullyGeneralTime: Option[AffineBigOCombo[MethodName]] = {
//    this.bestImplementationForConditions(ImplPredicateList.empty(impls.head.lhs.parameters.length)).map(_.rhs)
//  }
}

object SingleMethodImplSet {
  def fromSet(set: Set[UnnamedImpl], map: Map[UnnamedImpl, ImplSource]): SingleMethodImplSet = {
    val df = DominanceFrontier.fromSet(set)
    SingleMethodImplSet(df, map.filterKeys((x) => df.items.contains(x)))
  }

  def fromSetOfTuples(set: Set[(UnnamedImpl, ImplSource)]): SingleMethodImplSet = {
    fromSet(set.map(_._1), set.toMap)
  }
}
