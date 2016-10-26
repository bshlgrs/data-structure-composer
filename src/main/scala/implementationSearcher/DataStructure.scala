package implementationSearcher

import parsers.MainParser

case class DataStructure(parameters: List[MethodName], conditions: ImplPredicateMap, impls: Set[Impl]) {
  def isSimple: Boolean = parameters.isEmpty

//  def searchResult: UnfreeImplSet = UnfreeImplSet.fromSetOfUnfreeImpls(impls)

  def readMethods: Set[Impl] = {
    impls.filterNot(_.lhs.name.isMutating)
  }

  def writeMethods: Set[Impl] = {
    impls.filter(_.lhs.name.isMutating)
  }
}


object DataStructure {
  // todo: consider what happens when the data structures aren't simple
  def combineReadMethods(dataStructures: Set[DataStructure]): UnfreeImplSet = {
    UnfreeImplSet.fromSetOfUnfreeImpls(dataStructures.flatMap(_.sourcedImpls))
  }

  def apply(string: String): DataStructure = MainParser.nakedDataStructure.parse(string).get.value
}
