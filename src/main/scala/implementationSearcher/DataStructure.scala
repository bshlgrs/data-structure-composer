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
  val library = Map(
    "ArrayList" -> SimpleDataStructure("ArrayList", Set(
      Impl(ImplLhs("getByIndex"), AffineBigOCombo(ConstantTime, Map())),
      Impl(ImplLhs("insertAtEnd!"), AffineBigOCombo(ConstantTime, Map())),
      Impl(ImplLhs("deleteLast!"), AffineBigOCombo(ConstantTime, Map())),
      Impl(ImplLhs("updateNode!"), AffineBigOCombo(ConstantTime, Map()))
    )),
    "ReadOnlyLinkedList" -> SimpleDataStructure("ReadOnlyLinkedList", Set(
      Impl(ImplLhs("getFirst"), AffineBigOCombo(ConstantTime, Map())),
      Impl(ImplLhs("getNext"), AffineBigOCombo(ConstantTime, Map()))
    ))
  )


}
