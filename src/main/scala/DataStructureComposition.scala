/**
  * Created by buck on 5/1/16.
  */
class DataStructureComposition(implementations: Map[Method, Implementation], structures: Set[DataStructure]) {
  def timeForMethod(method: Method): Option[BigOExpression] = implementations(method).totalTimeFromImplementations(implementations)

  def totalTimeForDesiderata(desiderata: Map[Method, BigOExpression]): Option[BigOExpression] = {
    ???
  }
}

object DataStructureComposition {
  def createFromDesiderata(structures: Set[DataStructure], desiderata: Map[Method, BigOExpression]): Option[DataStructureComposition] = {
    ???
  }
}