package implementationSearcher

/**
  * Created by buck on 7/26/16.
  */
abstract class ImplSource {
}

case class DataStructureSource(str: String) extends ImplSource {

}

case object TopLevelImplSource extends ImplSource

case class ApplicationSource(impl: Impl) extends ImplSource

case class ProduceSource(lhs: ImplSource, rhs: ImplSource) extends ImplSource
//case class DataStructureSource(structure: DataStructureShell) extends ImplSource {
//  def parameters = structure.parameters
//}
