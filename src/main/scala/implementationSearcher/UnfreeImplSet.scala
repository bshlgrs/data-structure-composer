package implementationSearcher

import implementationSearcher.ImplLibrary.Decls
import shared._
import com.softwaremill.quicklens._
import org.scalactic.TypeCheckedTripleEquals._

import scala.scalajs.js.annotation.JSExport
/**
  * Created by buck on 9/10/16.
  */
case class UnfreeImplSet(@JSExport impls: Map[MethodName, SingleMethodImplSet], @JSExport boundVariables: Set[MethodName]) {
  def addImpl(impl: BoundImpl): UnfreeImplSet = {
    if (impls.contains(impl.name)) {
      this.modify(_.impls).using(_.updated(impl.name, impls(impl.name).add(impl)))
    } else {
      this.modify(_.impls).using(_ ++ Map(impl.name -> SingleMethodImplSet.fromSet(Set(impl))))
    }
  }

  def addImpls(impls: Set[BoundImpl]): UnfreeImplSet = {
    impls.foldLeft(this)((s, u) => s.addImpl(u))
  }

  def filterMethods(includedMethods: MethodName*): UnfreeImplSet =
    this.modify(_.impls).using(_.filterKeys((x) => includedMethods.contains(x)))

  def filterToAdt(adt: AbstractDataType): UnfreeImplSet =
    this.modify(_.impls).using(_.filterKeys((x) =>
      adt.methods.keys.toSeq.map(_.name).contains(x))
    )

  def contains(impl: Impl): Boolean = impls.get(impl.name).exists(_.impls.exists(_.impl === impl.unnamed))

  def filterToReadMethods: UnfreeImplSet = this.modify(_.impls).using(_.filterKeys(_.isRead))

  lazy val allImpls: Set[BoundImpl] = {
    impls.flatMap({ case ((name, singleMethodImplSet)) => singleMethodImplSet.impls.map(_.withName(name)) }).toSet
  }

  def isOtherImplUseful(impl: Impl): Boolean = {
    impls.get(impl.name) match {
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
  def implsWhichMatchMethodExpr(methodExpr: MethodExpr, list: ParameterList, decls: Decls): Set[(ImplPredicateMap, Impl.Rhs, BoundUnnamedImpl)] = {
    if (impls.contains(methodExpr.name)) {
      impls(methodExpr.name).implsWhichMatchMethodExpr(methodExpr, this, list, decls)
    } else {
      Set()
    }
  }

  def namedImplsWhichMatchMethodExpr(methodExpr: MethodExpr, list: ParameterList, decls: Decls): Set[(ImplPredicateMap, Impl.Rhs, BoundImpl)] = {
    implsWhichMatchMethodExpr(methodExpr, list, decls).map((x) => (x._1, x._2, x._3.withName(methodExpr.name)))
  }

  def toLongString: String = {
    "Search Result {\n" + impls.toList.sortBy(_._1.name)
      .map({ case (name, set) => "  " ++ name.name ++ ": " ++ set.toLongString})
      .mkString("\n") + "\n}"
  }

  def get(methodName: MethodName): Set[BoundUnnamedImpl] = impls.get(methodName).map(_.impls).getOrElse(Set())

  def getNamed(methodName: MethodName): Set[BoundImpl] = get(methodName).map(_.withName(methodName))

  def getNamedWithoutSource(methodName: MethodName): Set[Impl] = getNamed(methodName).map(_.impl)

  def partialCompareWithTime(other: UnfreeImplSet): DominanceRelationship = {
    PartialOrdering.fromSetOfDominanceRelationships(
      (this.impls.keys ++ other.impls.keys).map((key) => (this.impls.get(key), other.impls.get(key)) match {
        case (Some(xRes), Some(yRes)) => xRes.partialCompare(yRes)
        case (Some(_), None) => LeftStrictlyDominates
        case (None, Some(_)) => RightStrictlyDominates
        case (None, None) => BothDominate
      }).toSet
    )
  }

  def product(other: UnfreeImplSet): UnfreeImplSet = {
    UnfreeImplSet(this.impls.flatMap({ case ((methodName, singleMethodImplOptions)) =>
      if (other.impls.contains(methodName)) {
        Some(methodName -> singleMethodImplOptions.product(other.impls(methodName)))
      } else {
        Nil
      }
    }), boundVariables ++ other.boundVariables)
  }

  def getMatchingImpl(impl: Impl): Option[BoundImpl] = {
    impls(impl.name).options.items.find(_.impl === impl.unnamed).map(_.withName(impl.name))
  }

  def getMatchingImplFromLhs(implLhs: ImplLhs): Option[BoundImpl] = {
    impls(implLhs.name)
      .options
      .items
      .find(_.impl.predicates === implLhs.conditions)
      .map(_.withName(implLhs.name))
  }
}


object UnfreeImplSet {
  def fromCleanSet(impls: Set[BoundImpl], vars: Set[MethodName]) = UnfreeImplSet(Map(), vars).addImpls(impls)
}
