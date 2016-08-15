package firstNewTry

/**
  * Created by buck on 5/11/16.
  */
case class ImplementationInstantiation(implementation: Implementation, dataStructure: Option[DataStructureInstantiation]) {
  def methodName = implementation.methodName
}
