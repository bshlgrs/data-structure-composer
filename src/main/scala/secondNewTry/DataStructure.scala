package secondNewTry

import shared.ConstantTime

/**
  * Created by buck on 7/26/16.
  */
case class SimpleDataStructure(name: String, impls: List[Impl]) {
}

object DataStructureLibrary {
  val Vector = List()
  val library = Set(
    SimpleDataStructure("ArrayList", List(
      Impl(ImplLhs("getByIndex"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("insertAtEnd!"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("deleteLast!"), ImplRhs(ConstantTime)),
      Impl(ImplLhs("updateNode!"), ImplRhs(ConstantTime))
    ))
  )
}
