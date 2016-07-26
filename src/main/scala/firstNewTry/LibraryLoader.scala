package firstNewTry

import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success, Try}

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

  private def parseFile(path: String): Try[(List[Impl], List[DataStructure])] = {
    val text = Source.fromFile(path).getLines.toList.mkString("\n")

    new ImplParser(text).InputImplementationFile.run() match {
      case Success(x) => Success(x)
      case Failure(x: ParseError) => Failure(new RuntimeException(new ImplParser(text).formatError(x)))
      case Failure(x) => Failure(x)
    }
  }
}
