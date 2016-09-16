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

  // The more conditions on the MethodExpr, the better.


  // So I want to find all of the ways that I can implement `myMethod[f]` where I know `f.bar` to be true.
  // If I see `myMethod[g] if g.foo <- log(n)`, I want to return that.
  // If I see `myMethod[g] if g.bar <- log(n**2)`, I want to return that.

  // If instead of `myMethod[f]` I was calling this with `myMethod[_{foo}]`:
  // If I see `myMethod[g] if g.foo <- log(n)`, I want to return that.
  // If I see `myMethod[g] if g.bar <- log(n**2)`, I want to not include that one. !!!
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, implPredicateMap: ImplPredicateMap): Set[(UnfreeImpl, ImplPredicateMap)] = {
    if (impls.contains(methodExpr.name)) {
      impls(methodExpr.name).implsWhichMatchMethodExpr(methodExpr, implPredicateMap)
    } else {
      Set()
    }
  }

  def toLongString: String = {
    "Search Result {\n" + impls.values.map(_.toLongString).mkString("\n") + "\n}"
  }
}
