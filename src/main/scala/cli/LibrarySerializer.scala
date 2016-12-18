package cli

import implementationSearcher.LibraryText
import upickle.default


/**
  * Created by buck on 12/17/16.
  */
object LibrarySerializer {
  def main(args: Array[String]): Unit = {
    println(default.write(SerializedLibrary(DataStructureChooserCli.libraryText, DataStructureChooserCli.dataStructuresFiles)))
  }
}

case class SerializedLibrary(libraryText: LibraryText, dataStructureFiles: Set[String])
