import implementationSearcher.{Chooser, DataStructure}
import parsers.MainParser

import scala.io.Source

/**
  * Created by buck on 10/11/16.
  */
object DataStructureChooserCli {
  lazy val libraryText = {
    Source.fromFile("data/implementations.txt")
      .getLines()
      .mkString("\n")
  }

  lazy val autoImplLibrary = MainParser.impls.parse(libraryText).get.value.toSet

  lazy val dataStructuresText = {
    Source.fromFile("data/data_structures.txt")
      .getLines()
      .mkString("\n")
  }

//  lazy val dataStructuresLibrary: Map[String, DataStructure] = {
//    MainParser.dataStructureFile.parse(dataStructuresText).get.value.map((x) => x.name -> x).toMap
//  }

//  def main(args: Array[String]) {
//    val adt = MainParser.nakedAdt.parse("adt List { getFirst -> 1; }").get.value
//
//    println(Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(
//      autoImplLibrary,
//      dataStructuresLibrary.values.toSet,
//      adt))
//  }
}
