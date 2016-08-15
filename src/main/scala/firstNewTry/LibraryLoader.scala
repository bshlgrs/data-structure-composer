package firstNewTry

import scala.io.Source
import scala.util.Try

/**
  * Created by buck on 5/9/16.
  */
object LibraryLoader {
  def main(args: Array[String]) {
    library.implementations.foreach(println)
    library.structures.foreach(println)
  }

  lazy val library: Library = {
    (for {
      (_, dataStructures) <- parseFile("./materials/implementations/DataStructures.txt")
      (implementations, _) <- parseFile("./materials/implementations/MainImplementations.txt")
    } yield Library(implementations, dataStructures)).get
  }

  private def parseFile(path: String): Try[(List[Implementation], List[DataStructure])] = {
    val text = Source.fromFile(path).getLines.toList.mkString("\n")

    new ImplementationParser(text).InputImplementationFile.run()
  }
}
