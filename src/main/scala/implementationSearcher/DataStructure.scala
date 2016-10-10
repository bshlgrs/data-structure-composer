package implementationSearcher

import parsers.MainParser
import shared._

import scala.PartialOrdering

/**
  * Created by buck on 7/26/16.
  */

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

case class DataStructureChoice(choices: Set[String], results: Map[MethodName, AffineBigOCombo[MethodName]]) {

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

object DataStructureLibrary {
//  val ArrayList = SimpleDataStructure("ArrayList", Set(
//    UnfreeImpl("getByIndex <- 1"),
//    UnfreeImpl("insertAtEnd! <- 1"),
//    UnfreeImpl("deleteLast! <- 1"),
//    UnfreeImpl("updateNode! <- 1")
//  ))
//
//  val ReadOnlyLinkedList = SimpleDataStructure("ReadOnlyLinkedList", Set(
//    UnfreeImpl(ImplLhs("getFirst"), AffineBigOCombo(ConstantTime, Map())),
//    UnfreeImpl(ImplLhs("getNext"), AffineBigOCombo(ConstantTime, Map()))
//  ))
//
//  val SumMemoizer = SimpleDataStructure("SumMemoizer", Set(
//    UnfreeImpl(ImplLhs("getSum"), AffineBigOCombo(ConstantTime, Map())),
//    UnfreeImpl(ImplLhs("insertNode"), AffineBigOCombo(ConstantTime, Map()))
//  ))
//
//  val library = Map(
//    "ArrayList" -> ArrayList,
//    "ReadOnlyLinkedList" -> ReadOnlyLinkedList,
//    "SumMemoizer" -> SumMemoizer
//  )
}

