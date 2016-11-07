package implementationSearcher

import shared._

import scala.PartialOrdering

/**
  * Created by buck on 10/10/16.
  */

case class DataStructureChoice(choices: Set[String], times: UnfreeImplSet, adt: AbstractDataType) {
  lazy val overallTimeForAdt: BigOLiteral = {
//    assert(results.keys.forall(_.args.isEmpty),
//       s"While calculating the overall time for an ADT on the data structure choice $this, " +
//         s"I noticed that some of your results ")

    adt.methods.keys.map((methodName) => resultTimes(methodName) * adt.methods(methodName)).reduce(_ + _)
  }

  lazy val results: Map[MethodExpr, BoundImpl] = {
    adt.methods.keys.map((methodExpr: MethodExpr) => {
      // TODO: let this be a proper dominance frontier
      methodExpr -> times.implsWhichMatchMethodExpr(methodExpr, ParameterList.empty).head.withName(methodExpr.name)
    }).toMap
  }

  lazy val resultTimes: Map[MethodExpr, BigOLiteral] = {
    results.mapValues(_.impl.rhs.mapKeys(_.getAsNakedName).substituteAllVariables(adt.parameters))
  }
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
}
