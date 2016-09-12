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
      Impl(ImplLhs("getByIndex"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("insertAtEnd!"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("deleteLast!"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("updateNode!"), ImplRhs(ConstantTime))
    )),
    "ReadOnlyLinkedList" -> SimpleDataStructure("ReadOnlyLinkedList", Set(
      Impl(ImplLhs("getFirst"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("getNext"), ImplRhs(ConstantTime))
    ))
  )


}
