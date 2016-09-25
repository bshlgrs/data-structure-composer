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
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, implLhs: ImplLhs, searchResult: SearchResult): Set[(UnfreeImpl, ImplPredicateMap, AffineBigOCombo[MethodName])] =
    options.flatMap({ (option) =>
      option.bindToContext(methodExpr, implLhs, searchResult).map((x) => (option, x._1, x._2))
    })

  // More impl conditions means that this function returns something better
  def implsWhichMatchConditions(implPredicates: ImplPredicateList): Set[UnfreeImpl] = {
    options.filter(_.compatibleWithConditions(implPredicates))
  }

  def toLongString: String = {
    val startLhs = options.head.lhs
    s"  ${ImplLhs(startLhs.name.name, startLhs.parameters).toString} { " + options.toList.map((unfreeImpl) => {
      s"    (${unfreeImpl.lhs.conditions.toNiceString(startLhs.parameters)}) <- ${unfreeImpl.rhs}"
    }).mkString("\n") + "  }"
  }

  def sum(other: SingleMethodImplOptions): SingleMethodImplOptions = {
    SingleMethodImplOptions.fromSet(this.options ++ other.options)
  }

  def product(other: SingleMethodImplOptions): SingleMethodImplOptions = {
    assert(this.name == other.name)

    SingleMethodImplOptions.fromSet(for {
      x <- this.options
      y <- other.options
    } yield {
      val translatedY = y.alphaConvert(x.lhs.parameters)
      val lhs = x.lhs.addConditions(y.lhs.conditions)
      val rhs = x.rhs + y.rhs
      UnfreeImpl(lhs, rhs, x.source)
    })
  }

  def bestFullyGeneralTime: Option[AffineBigOCombo[MethodName]] = {
    this.bestImplementationForConditions(ImplPredicateList.empty(options.head.lhs.parameters.length)).map(_.rhs)
  }
}

object SingleMethodImplOptions {
  def fromSet(set: Set[UnfreeImpl]): SingleMethodImplOptions = {
    set.foldLeft(SingleMethodImplOptions(Set(set.head)))(
      (smio, unfreeImpl) => smio.add(unfreeImpl))
  }
}
