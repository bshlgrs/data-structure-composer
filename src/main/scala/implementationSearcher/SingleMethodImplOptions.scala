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
      // TODO: This is really stupid
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
        if (currentBest.cost > unfreeImpl.cost) {
          true
        } else {
          false
        }
      }
      case None => true
    }
  }

  // More impl predicates means that this function returns something better
  def implsWhichMatchConditions(implPredicates: ImplPredicateList): Set[UnfreeImpl] = {
    options.filter((u: UnfreeImpl) => u.compatibleWithConditions(implPredicates))
  }

  def toLongString: String = {
    s"  $name {\n" + options.toList.map((unfreeImpl) => s"    ${unfreeImpl.lhs.conditions} <- ${unfreeImpl.cost}").mkString("\n") + "\n  }"
  }
}
