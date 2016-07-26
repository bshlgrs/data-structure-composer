package firstNewTry

/**
  * Created by buck on 5/11/16.
  */
case class DataStructureInstantiation(dataStructure: DataStructure, parameters: Map[String, FunctionExpr]) {
  override def toString = dataStructure.name
}
