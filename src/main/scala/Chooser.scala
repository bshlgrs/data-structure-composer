import java.io.File

/**
  * Created by buck on 5/2/16.
  */
object Chooser {
  def main (args: Array[String]) {
    println(new File(".").getAbsolutePath)

  }

  val methods: Set[Method] = Methods.methods
  val universalImplementations: Set[UniversalImplementation] = UniversalImplementations.universalImplementations
  val dataStructures: Set[DataStructure] = DataStructures.dataStructures

  val dataStructurePowerSet: Set[Set[DataStructure]] = dataStructures.subsets.toSet

  def choose(desiderata: Map[Method, BigOExpression]): DataStructureComposition = {
    dataStructurePowerSet
      .flatMap(DataStructureComposition.createFromDesiderata(_, desiderata))
      .maxBy(_.totalTimeForDesiderata(desiderata))
  }
}
