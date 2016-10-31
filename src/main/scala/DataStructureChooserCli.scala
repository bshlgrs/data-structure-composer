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

  lazy val (impls, decls) = MainParser.parseImplFileString(libraryText).get

  lazy val dataStructuresText = {
    Source.fromFile("data/data_structures.txt")
      .getLines()
      .mkString("\n")
  }

  lazy val dataStructuresLibrary: Map[String, DataStructure] = {
    MainParser.parseDataStructureFileString(dataStructuresText, impls, decls).get
  }

  def main(args: Array[String]) {
    val adt = MainParser.nakedAdt.parse("adt List { getByIndex -> 1; insertAtIndex! -> 1; }").get.value

    println("hello")
    println(Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(
      impls,
      dataStructuresLibrary,
      decls,
      adt))
  }
}
