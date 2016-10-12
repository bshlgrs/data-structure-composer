package implementationSearcher

import shared._

import scala.PartialOrdering

/**
  * Created by buck on 10/10/16.
  */

case class DataStructureChoice(choices: Set[String], results: Map[MethodExpr, AffineBigOCombo[MethodName]]) {
  def overallTimeForAdt(abstractDataType: AbstractDataType): BigOLiteral = {
    assert(results.values.forall(_.weights.isEmpty))

    abstractDataType.methods.keys.map((methodName) =>
      results(methodName).substituteAllVariables(abstractDataType.parameters) * abstractDataType.methods(methodName)).reduce(_ + _)
  }
}

object DataStructureChoice {
  implicit object DataStructureChoicePartialOrdering extends shared.PartialOrdering[DataStructureChoice] {
    def partialCompare(x: DataStructureChoice, y: DataStructureChoice): DominanceRelationship = {
      val simplicityComparison = PartialOrdering.fromSetsOfProperties(x.choices, y.choices).flip
      val timeComparison = PartialOrdering.fromSetOfDominanceRelationships(
        (x.results.keys ++ y.results.keys).map((key) => (x.results.get(key), y.results.get(key)) match {
          case (Some(xRes), Some(yRes)) => xRes.partialCompare(yRes)
          case (Some(_), None) => LeftStrictlyDominates
          case (None, Some(_)) => RightStrictlyDominates
          case (None, None) => NeitherDominates
        }).toSet
      )

      timeComparison.orIfTied(simplicityComparison)
    }
  }
}
