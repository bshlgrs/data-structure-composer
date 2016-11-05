package implementationSearcher

import shared.BigOLiteral


/**
  * Created by buck on 9/10/16.
  */
case class UnfreeImplSet(impls: Map[MethodName, SingleMethodImplSet], boundVariables: Set[MethodName], declarations: Map[MethodName, ImplDeclaration]) {
  def addImpl(impl: Impl): UnfreeImplSet = {
    if (impls.contains(impl.lhs.name)) {
      this.copy(impls = impls.updated(impl.lhs.name, impls(impl.lhs.name).add(impl)))
    } else {
      this.copy(impls = impls ++ Map(impl.lhs.name -> SingleMethodImplSet.fromSetOfImplsWithSource(Set(implWs))))
    }
  }

  def addImpls(impls: Set[Impl]): UnfreeImplSet = {
    impls.foldLeft(this)((s, u) => s.addImpl(u))
  }

  def filterMethods(includedMethods: MethodName*): UnfreeImplSet =
    this.copy(impls = impls.filterKeys((x) => includedMethods.contains(x)))

  def filterToAdt(adt: AbstractDataType): UnfreeImplSet = this.copy(impls =
    impls.filterKeys((x) => adt.methods.keys.toSeq.map(_.name).contains(x)))

  def allImpls: Set[Impl] = {
    impls.flatMap({ case ((name, singleMethodImplSet)) =>
      singleMethodImplSet.withNameAndSources(name)
    }).toSet
  }

  def isOtherImplUseful(impl: Impl): Boolean = {
    impls.get(impl.lhs.name) match {
      case Some(set: SingleMethodImplSet) => set.isOtherImplUseful(impl)
      case None => true
    }
  }

  // The more conditions on the MethodExpr, the better.

  // So I want to find all of the ways that I can implement `myMethod[f]` where I know `f.bar` to be true.
  // If I see `myMethod[g] if g.foo <- log(n)`, I want to return that.
  // If I see `myMethod[g] if g.bar <- log(n**2)`, I want to return that.

  // If instead of `myMethod[f]` I was calling this with `myMethod[_{foo}]`:
  // If I see `myMethod[g] if g.foo <- log(n)`, I want to return that.
  // If I see `myMethod[g] if g.bar <- log(n**2)`, I want to not include that one. !!!
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, list: ParameterList): Set[UnnamedImpl] = {
    if (impls.contains(methodExpr.name)) {
      impls(methodExpr.name).implsWhichMatchMethodExpr(methodExpr, this, list)
    } else {
      Set()
    }
  }

  def toLongString: String = {
    "Search Result {\n" + impls.toList.sortBy(_._1.name)
      .map({ case (name, set) => "  " ++ name.name ++ ": " ++ set.toLongString})
      .mkString("\n") + "\n}"
  }

  def get(methodName: MethodName): Set[UnnamedImpl] = impls.get(methodName).map(_.impls).getOrElse(Set())

  def getNamed(methodName: MethodName): Set[Impl] = get(methodName).map(_.withName(methodName))

  def product(other: UnfreeImplSet): UnfreeImplSet = {
    UnfreeImplSet(this.impls.flatMap({ case ((methodName, singleMethodImplOptions)) =>
      if (other.impls.contains(methodName)) {
        Some(methodName -> singleMethodImplOptions.product(other.impls(methodName)))
      } else {
        Nil
      }
    }), boundVariables ++ other.boundVariables, declarations ++ other.declarations)
  }

//  def bestFullyGeneralTimes: Map[MethodName, AffineBigOCombo[MethodName]] = {
//    impls.mapValues((x) => x.bestFullyGeneralTime).filter(_._2.isDefined).mapValues(_.get)
//  }
}


