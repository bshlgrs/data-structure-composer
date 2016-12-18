package cli

import implementationSearcher._
import parsers.MainParser
import shared.DominanceFrontier

import scala.io.Source

/**
  * Created by buck on 10/11/16.
  */
object DataStructureChooserCli {
  lazy val implsText = {
    Source.fromFile("data/implementations.txt")
      .getLines()
      .mkString("\n")
  }

  val dataStructuresFiles: Set[String] = {
    new java.io.File("data/data_structures").list().map((fileName: String) => {
      Source.fromFile("data/data_structures/" + fileName)
        .getLines()
        .mkString("\n")
    }).toSet
  }


  lazy val (library, libraryText) = ImplLibrary.buildFromStrings(implsText, dataStructuresFiles).get

  val ImplLibrary(impls, decls, structures) = library

  def chooseDataStructures(adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    time {
      Searcher.allMinTotalCostParetoOptimalDataStructureCombosForAdt(
        library,
        adt)
    }
  }

  def chooseAllDataStructures(adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    time {
      Searcher.allParetoOptimalDataStructureCombosForAdt(
        library,
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
      choice.structureWriteMethods.mkString(",") ++ "\n" ++
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

