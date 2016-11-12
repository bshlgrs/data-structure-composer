package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import shared.{DominanceRelationship, PartialOrdering}

class ImplPredicateMap(dirtyMap: Map[MethodName, Set[FunctionProperty]]) {
  val map: Map[MethodName, Set[FunctionProperty]] = dirtyMap.filter(_._2.nonEmpty)

  def and(other: ImplPredicateMap): ImplPredicateMap = {
    ImplPredicateMap(
      (map.keys ++ other.map.keys).map((parameterName: MethodName) =>
        parameterName -> map.getOrElse(parameterName, Set()).union(other.map.getOrElse(parameterName, Set()))
      ).toMap
    )
  }

  def isEmpty: Boolean = map.values.forall(_.isEmpty)

  def get(name: MethodName): Set[FunctionProperty] = map.getOrElse(name, Set())

  lazy val toNiceString: String =
    map.flatMap({ case (name: MethodName, y: Set[FunctionProperty]) => y.map((z) => s"${name.name}.$z") }).mkString(", ")

  override def equals(obj: scala.Any): Boolean = obj match {
    case x: ImplPredicateMap => x.map == map
    case _ => false
  }

  override def hashCode(): Int = map.hashCode() ^ ImplPredicateMap.hashCode()
}

object ImplPredicateMap {
  def fromListOfTuples(conditions: List[(String, String)]): ImplPredicateMap = {
    val map1: Map[MethodName, Set[(String, String)]] = conditions.toSet.groupBy({
      case ((paramName: String, condition: String)) =>
        MethodName(paramName)
    })

    val map2: Map[MethodName, Set[FunctionProperty]] = map1.mapValues(_.map(_._2))

    ImplPredicateMap.apply(map2)
  }

  def apply(map: Map[MethodName, Set[FunctionProperty]]): ImplPredicateMap = {
    new ImplPredicateMap(map.filter(_._2.nonEmpty))
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
