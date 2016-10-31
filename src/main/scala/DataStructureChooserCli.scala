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
    val adt = MainParser.nakedAdt.parse("adt List { getMinimum -> 1; insertAtIndex! -> 1; }").get.value

    val res = time {
      Chooser.allParetoOptimalDataStructureCombosForAdt(
        impls,
        dataStructuresLibrary,
        decls,
        adt)
    }

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

