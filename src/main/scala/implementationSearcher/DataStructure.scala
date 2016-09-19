package implementationSearcher

import shared.ConstantTime

/**
  * Created by buck on 7/26/16.
  */
case class SimpleDataStructure(name: String, impls: Set[Impl]) {
  def sourcedImpls: Set[Impl] = {
    impls.map((x) => Impl(x.lhs, x.rhs, Some(DataStructureSource(name))))
  }
}

object DataStructureLibrary {
  val ArrayList = SimpleDataStructure("ArrayList", Set(
    Impl("getByIndex <- 1"),
    Impl("insertAtEnd! <- 1"),
    Impl("deleteLast! <- 1"),
    Impl("updateNode! <- 1")
  ))

  val ReadOnlyLinkedList = SimpleDataStructure("ReadOnlyLinkedList", Set(
    Impl(ImplLhs("getFirst"), AffineBigOCombo(ConstantTime, Map())),
    Impl(ImplLhs("getNext"), AffineBigOCombo(ConstantTime, Map()))
  ))

  val SumMemoizer = SimpleDataStructure("SumMemoizer", Set(
    Impl(ImplLhs("getSum"), AffineBigOCombo(ConstantTime, Map())),
    Impl(ImplLhs("insertNode"), AffineBigOCombo(ConstantTime, Map()))
  ))

  val library = Map(
    "ArrayList" -> ArrayList,
    "ReadOnlyLinkedList" -> ReadOnlyLinkedList,
    "SumMemoizer" -> SumMemoizer
  )
}

