package implementationSearcher

import shared.{DominanceFrontier, BigOLiteral}

/**
  * Created by buck on 9/10/16.
  */
case class SingleMethodImplOptions(options: DominanceFrontier[UnfreeImpl]) {
  assert(impls.forall((u: UnfreeImpl) => u.lhs.name == name),
    s"SingleMethodImplOptions should only be for a single method, but you have the following unfreeImpls: $options" +
      s"with names ${impls.map(_.lhs.name)} and name $name")

  def name: MethodName = impls.head.lhs.name

  def impls = options.items

  def bestImplementationForConditions(conditions: ImplPredicateList): Option[UnfreeImpl] = {
    implsWhichMatchConditions(conditions).toList.sortBy(_.cost).headOption
  }

  def add(unfreeImpl: UnfreeImpl): SingleMethodImplOptions = {
    SingleMethodImplOptions(options.add(unfreeImpl))
  }

  def isOtherImplUseful(unfreeImpl: UnfreeImpl): Boolean = {
    bestImplementationForConditions(unfreeImpl.lhs.conditions) match {
      case Some(currentBest) => {
        currentBest.cost > unfreeImpl.cost
      }
      case None => true
    }
  }

  // More impl conditions means that this function returns something better
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, scope: Scope): Set[(UnfreeImpl, ImplPredicateMap, AffineBigOCombo[MethodName])] =
    impls.flatMap({ (option) =>
      option.bindToContext(methodExpr, scope).map((x) => (option, x._1, x._2))
    })

  // More impl conditions means that this function returns something better
  def implsWhichMatchConditions(implPredicates: ImplPredicateList): Set[UnfreeImpl] = {
    impls.filter(_.compatibleWithConditions(implPredicates))
  }

  def toLongString: String = {
    val startLhs = impls.head.lhs
    s"  ${ImplLhs(startLhs.name.name, startLhs.parameters).toString} { " + impls.toList.map((unfreeImpl) => {
      s"    (${unfreeImpl.lhs.conditions.toNiceString(startLhs.parameters)}) <- ${unfreeImpl.rhs}"
    }).mkString("\n") + "  }"
  }

  def sum(other: SingleMethodImplOptions): SingleMethodImplOptions = {
    SingleMethodImplOptions(this.options ++ other.options)
  }

  def product(other: SingleMethodImplOptions): SingleMethodImplOptions = {
    assert(this.name == other.name)

    SingleMethodImplOptions.fromSet(for {
      x <- this.impls
      y <- other.impls
    } yield {
      val translatedY = y.alphaConvert(x.lhs.parameters)
      val lhs = x.lhs.addConditions(y.lhs.conditions)
      val rhs = x.rhs + y.rhs
      UnfreeImpl(lhs, rhs, x.source)
    })
  }

  def bestFullyGeneralTime: Option[AffineBigOCombo[MethodName]] = {
    this.bestImplementationForConditions(ImplPredicateList.empty(impls.head.lhs.parameters.length)).map(_.rhs)
  }
}

object SingleMethodImplOptions {
  def fromSet(set: Set[UnfreeImpl]): SingleMethodImplOptions = {
    SingleMethodImplOptions(DominanceFrontier.fromSet(set))
  }

}
