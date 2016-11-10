package implementationSearcher

import shared._

import scala.PartialOrdering

/**
  * Created by buck on 10/10/16.
  */

case class DataStructureChoice(choices: Set[DataStructure],
                               times: Set[BoundImpl],
                               adt: AbstractDataType,
                               results: Map[MethodExpr, BoundImpl],
                               freeImpls: Set[FreeImpl]) {
  lazy val overallTimeForAdt: BigOLiteral = {
//    assert(results.keys.forall(_.args.isEmpty),
//       s"While calculating the overall time for an ADT on the data structure choice $this, " +
//         s"I noticed that some of your results ")

    adt.methods.keys.map((methodName) => resultTimes(methodName) * adt.methods(methodName)).reduce(_ + _)
  }

  lazy val resultTimes: Map[MethodExpr, BigOLiteral] = {
    results.mapValues(_.impl.rhs.mapKeys(_.getAsNakedName).substituteAllVariables(adt.parameters))
  }

  def choiceNames: Set[String] = choices.map(_.name)

  val freeImplSourceMap: Map[String, FreeImplSource] = freeImpls.map((x) => x.impl.toString -> x.freeImplSource).toMap
//  lazy val mapFromResultsToSources: Map[MethodExpr, Set[String]] = {
//    results.keys.map((x) => x -> getDataStructureSources(x)).toMap
//  }
//
//  def getDataStructureSources(m: MethodExpr): Set[String] = {
//    results(m).boundSource.boundSources
//  }

  lazy val jsBoundHashCodeMap: Map[Int, BoundImpl] = times.map((x) => x.impl.hashCode() -> x).toMap
  lazy val jsFreeHashCodeMap: Map[Int, FreeImpl] = freeImpls.map((x) => x.impl.hashCode() -> x).toMap

  // todo: make things output themselves as strings so that it's easy to use some kind
  // of topological sort in the front end to explain everything.
  lazy val implStrings: Map[String, BoundImpl] = times.map((x) => x.impl.lhs.toString -> x).toMap
}

object DataStructureChoice {
  class DataStructureChoicePartialOrdering(library: ImplLibrary)
    extends shared.PartialOrdering[DataStructureChoice] {
    def partialCompare(x: DataStructureChoice, y: DataStructureChoice): DominanceRelationship = {
      val timeComparison = PartialOrdering.fromSetOfDominanceRelationships(
        (x.results.keys ++ y.results.keys).map((key) => (x.results.get(key), y.results.get(key)) match {
          case (Some(xRes), Some(yRes)) => xRes.impl.rhs.partialCompare(yRes.impl.rhs)
          case (Some(_), None) => LeftStrictlyDominates
          case (None, Some(_)) => RightStrictlyDominates
          case (None, None) => BothDominate
        }).toSet
      )

      val simplicityComparison =
        PartialOrdering.fromSetsOfProperties(x.choices, y.choices).flip.neitherToBoth
          .orIfTied(library.partialCompareSetFromExtensionRelations(x.choices, y.choices))

      timeComparison.orIfTied(simplicityComparison)
    }
  }

  def build(choices: Set[DataStructure], times: UnfreeImplSet, adt: AbstractDataType, library: ImplLibrary): DataStructureChoice = {
    val results = adt.methods.keys.map((methodExpr: MethodExpr) => {
      // TODO: let this be a proper dominance frontier
      methodExpr -> times.implsWhichMatchMethodExpr(methodExpr, ParameterList.empty, library.decls).head.withName(methodExpr.name)
    }).toMap

    DataStructureChoice(choices, times.allImpls, adt, results, library.impls ++ choices.flatMap(_.freeImpls))
  }
}
