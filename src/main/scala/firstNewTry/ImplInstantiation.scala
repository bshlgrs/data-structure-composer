package firstNewTry

/**
  * Created by buck on 5/11/16.
  */
case class ImplInstantiation(implementation: Impl, dataStructure: Option[DataStructureInstantiation], args: Option[List[FunctionExpr]]) {
  assert(args match {
    case None => true
    case Some(list) => list.length == implementation.parameters.length
  })

  def methodName = implementation.methodName
}
