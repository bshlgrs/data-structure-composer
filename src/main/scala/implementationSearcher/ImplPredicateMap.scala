package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import shared.{PartialOrdering, DominanceRelationship}

case class ImplPredicateMap(map: Map[MethodName, Set[FunctionProperty]]) {
  def and(other: ImplPredicateMap): ImplPredicateMap = {
    ImplPredicateMap(
      (map.keys ++ other.map.keys).map((parameterName: MethodName) =>
        parameterName -> map.getOrElse(parameterName, Set()).union(other.map.getOrElse(parameterName, Set()))
      ).toMap
    )
  }

  def isEmpty: Boolean = map.values.forall(_.isEmpty)

  // todo: unsafe
  def get(name: MethodName): Set[FunctionProperty] = map.getOrElse(name, Set())

  def toNiceString: String =
    map.flatMap({ case (name: MethodName, y: Set[FunctionProperty]) => y.map((z) => s"${name.name}.$z") }).mkString(", ")
}

object ImplPredicateMap {
  def fromListOfTuples(conditions: List[(String, String)]): ImplPredicateMap = {
    val map1: Map[MethodName, Set[(String, String)]] = conditions.toSet.groupBy({
      case ((paramName: String, condition: String)) =>
        MethodName(paramName)
    })

    val map2: Map[MethodName, Set[FunctionProperty]] = map1.mapValues(_.map(_._2))

    ImplPredicateMap(map2)
  }

  def empty: ImplPredicateMap = ImplPredicateMap(Map())

  // A dominates B if A can be used to implement B
  implicit object ImplPredicateMapPartialOrdering extends PartialOrdering[ImplPredicateMap] {
    def partialCompare(x: ImplPredicateMap, y: ImplPredicateMap): DominanceRelationship = {
      PartialOrdering.fromSetOfDominanceRelationships(
        (x.map.keys ++ y.map.keys).map({case (methodName: MethodName) =>
          PartialOrdering.fromSetsOfProperties(x.get(methodName), y.get(methodName)) })
      ).flip
    }
  }
}
