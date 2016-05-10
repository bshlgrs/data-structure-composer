/**
  * Created by buck on 5/9/16.
  */
case class Library(implementations: List[Implementation], structures: List[DataStructure]) {
  def structureWithName(name: String): Option[DataStructure] = {
    structuresMap.get(name)
  }

  lazy val structuresMap: Map[String, DataStructure] = {
    structures.map((s) => s.name -> s).toMap
  }

  lazy val methodNameToImplementationsMap = {
    implementations.groupBy(_.methodName)
  }

  def simpleImplementationsForMethod(methodName: String): List[Implementation] = {
    methodNameToImplementationsMap(methodName).filter(_.isSuperSimple)
  }
}
