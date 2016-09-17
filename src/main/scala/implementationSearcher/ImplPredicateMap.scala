package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty

case class ImplPredicateMap(map: Map[String, Set[FunctionProperty]]) {
  def and(other: ImplPredicateMap): ImplPredicateMap = {
    ImplPredicateMap(
      (map.keys ++ other.map.keys).toSet.map((parameterName: String) =>
        parameterName -> map.getOrElse(parameterName, Set()).union(other.map.getOrElse(parameterName, Set()))
      ).toMap
    )
  }

  def toList(parameters: List[String]): ImplPredicateList = {
    ImplPredicateList(parameters.map(map.getOrElse(_, Set())))
  }
}

object ImplPredicateMap {
  def fromListOfTuples(conditions: List[(String, String)]): ImplPredicateMap = {
    val map1: Map[String, Set[(String, String)]] = conditions.toSet.groupBy({
      case ((paramName: String, condition: String)) =>
        paramName
    })

    val map2: Map[String, Set[String]] = map1.mapValues(_.map(_._2))

    ImplPredicateMap(map2)
  }
}
