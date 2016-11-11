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

  lazy val (impls, decls): (Set[FreeImpl], ImplLibrary.Decls) = MainParser.parseImplFileString(libraryText).get

  val dataStructuresFiles: Set[String] = {
    new java.io.File("data/data_structures").list().map((fileName: String) => {
      Source.fromFile("data/data_structures/" + fileName)
        .getLines()
        .mkString("\n")
    }).toSet
  }

  lazy val (dataStructures: Map[String, DataStructure], dataStructureTexts: Map[String, (String, String)]) = {
    val dataStructureTuples =
      dataStructuresFiles.map((x: String) => MainParser.parseSingleDataStructureFileString(x, decls).get)

    val duplicationErrors = dataStructureTuples.groupBy(_._1).filter(_._2.size > 1)

    assert(duplicationErrors.isEmpty,
      s"There were data structures with duplicate names: ${duplicationErrors.keys.mkString(", ")}")

    val dataStructures = dataStructureTuples.map((x) => x._1 -> x._2).toMap

    (dataStructures, dataStructureTuples.map((x) => x._1 -> (x._3 -> x._4)).toMap)
  }

  lazy val library = ImplLibrary(impls, decls, dataStructures)

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

