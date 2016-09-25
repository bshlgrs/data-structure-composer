package implementationSearcher

import shared.BigOLiteral


/**
  * Created by buck on 9/10/16.
  */
case class SearchResult(impls: Map[MethodName, SingleMethodImplOptions] = Map(), freeVariables: Set[MethodName] = Set()) {
  def addImpl(impl: UnfreeImpl): SearchResult = {
    if (impls.contains(impl.lhs.name)) {
      SearchResult(impls.updated(impl.lhs.name, impls(impl.lhs.name).add(impl)))
    } else {
      SearchResult(impls ++ Map(impl.lhs.name -> SingleMethodImplOptions(Set(impl))))
    }
  }

  def addImpls(impls: Set[UnfreeImpl]): SearchResult = {
    impls.foldLeft(this)((s, u) => s.addImpl(u))
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
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, implLhs: ImplLhs, searchResult: SearchResult): Set[(UnfreeImpl, ImplPredicateMap, AffineBigOCombo[MethodName])] = {
    if (impls.contains(methodExpr.name)) {
      impls(methodExpr.name).implsWhichMatchMethodExpr(methodExpr, implLhs, searchResult)
    } else {
      Set()
    }
  }

  def toLongString: String = {
    "Search Result {\n" + impls.values.toList.sortBy(_.name.name).map(_.toLongString).mkString("\n") + "\n}"
  }

  def get(methodName: MethodName): Set[UnfreeImpl] = impls.get(methodName).map(_.options).getOrElse(Set())
  def get(name: String): Set[UnfreeImpl] = this.get(MethodName(name))

  def product(other: SearchResult): SearchResult = {
    SearchResult(this.impls.flatMap({ case ((methodName, singleMethodImplOptions)) =>
      if (other.impls.contains(methodName)) {
        Some(methodName -> singleMethodImplOptions.product(other.impls(methodName)))
      } else {
        Nil
      }
    }))
  }

  def bestFullyGeneralTimes: Map[MethodName, AffineBigOCombo[MethodName]] = {
    impls.mapValues((x) => x.bestFullyGeneralTime).filter(_._2.isDefined).mapValues(_.get)
  }
}


object SearchResult {
  def fromSetOfUnfreeImpls(unfreeImpls: Set[UnfreeImpl]): SearchResult = {
    SearchResult().addImpls(unfreeImpls)
  }
}
