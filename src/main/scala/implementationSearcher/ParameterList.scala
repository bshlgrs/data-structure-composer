package implementationSearcher

/**
  * Created by buck on 10/29/16.
  */
case class ParameterList(conditions: ImplPredicateMap, list: List[MethodName]) {
  assert(conditions.map.keys.forall(list.contains))

  def contains(name: MethodName): Boolean = list.contains(name)
}

object ParameterList {
  lazy val empty = ParameterList(ImplPredicateMap.empty, Nil)

  def easy(seq: MethodName *) = ParameterList(ImplPredicateMap.empty, seq.toList)
}
