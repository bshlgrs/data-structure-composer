package implementationSearcher

import shared.BigOLiteral

/**
  * Created by buck on 9/10/16.
  */
case class SingleMethodImplOptions(options: Set[UnfreeImpl]) {
  assert(options.forall((u: UnfreeImpl) => u.lhs.name == name),
    s"SingleMethodImplOptions should only be for a single method, but you have the following unfreeImpls: $options" +
      s"with names ${options.map(_.lhs.name)} and name $name")

  // TODO
  // This should be a dominance frontier, but I'm a terrible programmer, so it's not.

  def name: MethodName = options.head.lhs.name

  def bestImplementationForConditions(conditions: ImplPredicateList): Option[UnfreeImpl] = {
    implsWhichMatchConditions(conditions).toList.sortBy(_.cost).headOption
  }

  def add(unfreeImpl: UnfreeImpl): SingleMethodImplOptions = {
    bestImplementationForConditions(unfreeImpl.lhs.conditions) match {
      // TODO: This is really stupid--it doesn't correctly ensure that no items in the set are strictly dominated
      case Some(currentBest) => {
        if (currentBest.cost > unfreeImpl.cost) {
          SingleMethodImplOptions(options + unfreeImpl)
        } else {
          this
        }
      }
      case None => SingleMethodImplOptions(options + unfreeImpl)
    }
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
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, implPredicateMap: ImplPredicateMap): Set[(UnfreeImpl, ImplPredicateMap, AffineBigOCombo[MethodName])] =
    options.flatMap({ (option) =>
      option.bindToContext(methodExpr, implPredicateMap).map((x) => (option, x._1, x._2))
    })

  // More impl conditions means that this function returns something better
  def implsWhichMatchConditions(implPredicates: ImplPredicateList): Set[UnfreeImpl] = {
    options.filter(_.compatibleWithConditions(implPredicates))
  }

  def toLongString: String = {
    val startLhs = options.head.lhs
    s"  ${ImplLhs(startLhs.name.name, startLhs.parameters).toString} {\n" + options.toList.map((unfreeImpl) => {
      s"    (${unfreeImpl.lhs.conditions.toNiceString(startLhs.parameters)}) <- ${unfreeImpl.rhs}"
    }).mkString("\n") + "\n  }"
  }
}
