package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty


case class ImplPredicateList(list: List[Set[FunctionProperty]]) {
  def and(other: ImplPredicateList): ImplPredicateList = {
    assert(this.list.length == other.list.length, (this.list, other.list))

    ImplPredicateList(this.list.zip(other.list).map({ case ((x, y)) => x union y }))
  }

  def toNiceString(names: List[String]): String = {
    assert(names.length == list.length)

    list.zip(names).flatMap({ case ((set, name)) => set.toList.map(name + "." + _)}).mkString(", ")
  }

  def isEmpty: Boolean = list.forall(_.isEmpty)
}


case class ImplPredicate(parameterIdx: Int, property: String)
