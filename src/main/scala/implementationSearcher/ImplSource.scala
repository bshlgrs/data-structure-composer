package implementationSearcher

/**
  * Created by buck on 7/26/16.
  */
abstract class ImplSource {
  def parameters: Set[String]
}

case class StringSource(str: String) extends ImplSource {
  def parameters = Set()
}
//case class DataStructureSource(structure: DataStructureShell) extends ImplSource {
//  def parameters = structure.parameters
//}
