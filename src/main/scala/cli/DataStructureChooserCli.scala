package cli

import implementationSearcher._
import parsers.MainParser
import shared.DominanceFrontier

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

  lazy val dataStructures: Map[String, DataStructure] = {
    MainParser.parseDataStructureFileString(dataStructuresText, impls, decls).get
  }

  def chooseDataStructures(adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    time {
      Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(
        ImplLibrary(impls, decls, dataStructures),
        adt)
    }
  }

  def chooseAllDataStructures(adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    time {
      Chooser.allParetoOptimalDataStructureCombosForAdt(
        ImplLibrary(impls, decls, dataStructures),
        adt
      )
    }
  }

  def main(args: Array[String]) {
    val adt = MainParser.nakedAdt.parse("""
adt List {
  insertLast! -> 1
  deleteLast! -> 1
  getByIndex -> 1
  updateNode! -> 1
  getMinimum -> 1
}
    """.trim()).get.value

    val res = chooseDataStructures(adt)

    printResults(res)
  }

  def printResults(res: DominanceFrontier[DataStructureChoice]) = {
    println("Optimal results:")
    println(res.items.map((choice) => {
      choice.choices.mkString(",") ++ "\n" ++
        choice.results.toList.sortBy(_._1.name.name).map((tuple) => {
          "\t" ++ tuple._1.toString ++ " <- " ++ tuple._2.toString
        }).mkString("\n")
    }).mkString("\n"))
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

}

