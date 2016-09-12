package implementationSearcher

/**
  * Created by buck on 9/10/16.
  */
case class SearchResult(impls: Map[MethodName, SingleMethodImplOptions] = Map()) {
  def addImpl(impl: UnfreeImpl): SearchResult = {
    if (impls.contains(impl.lhs.name)) {
      SearchResult(impls.updated(impl.lhs.name, impls(impl.lhs.name).add(impl)))
    } else {
      SearchResult(impls ++ Map(impl.lhs.name -> SingleMethodImplOptions(Set(impl))))
    }
  }

  def allImpls: Set[UnfreeImpl] = impls.values.flatMap(_.options).toSet

  def isOtherImplUseful(unfreeImpl: UnfreeImpl): Boolean = {
    if (impls.contains(unfreeImpl.lhs.name)) {
      impls(unfreeImpl.lhs.name).isOtherImplUseful(unfreeImpl)
    } else {
      true
    }
  }

  def implsWhichMatchImplLhs(implLhs: ImplLhs): Set[UnfreeImpl] = {
    if (impls.contains(implLhs.name)) {
      impls(implLhs.name).implsWhichMatchConditions(implLhs.conditions)
    } else {
      Set()
    }
  }

  // The more conditions on the MethodExpr, the better.
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr): Set[UnfreeImpl] = {
    if (impls.contains(methodExpr.name)) {
      impls(methodExpr.name).implsWhichMatchConditions(methodExpr.conditions)
    } else {
      Set()
    }
  }

  def toLongString: String = {
    "Search Result {\n" + impls.values.map(_.toLongString).mkString("\n") + "\n}"
  }
}
