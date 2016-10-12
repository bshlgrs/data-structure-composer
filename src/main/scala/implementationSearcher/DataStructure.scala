package implementationSearcher

import parsers.MainParser

case class DataStructure(lhs: ImplLhs, impls: Set[UnfreeImpl]) {
  def name: String = lhs.name.name

  def isSimple: Boolean = lhs.parameters.isEmpty

  def sourcedImpls: Set[UnfreeImpl] = {
    impls.map((x) => UnfreeImpl(x.lhs, x.rhs, Some(DataStructureSource(name))))
  }

  def searchResult: SearchResult = SearchResult.fromSetOfUnfreeImpls(impls)

  def readMethods: Set[UnfreeImpl] = {
    impls.filterNot(_.lhs.name.isMutating)
  }

  def writeMethods: Set[UnfreeImpl] = {
    impls.filter(_.lhs.name.isMutating)
  }
}

object DataStructure {
  // todo: consider what happens when the data structures aren't simple
  def combineReadMethods(dataStructures: Set[DataStructure]): SearchResult = {
    SearchResult.fromSetOfUnfreeImpls(dataStructures.flatMap(_.sourcedImpls))
  }

  def apply(string: String): DataStructure = MainParser.nakedDataStructure.parse(string).get.value
}
