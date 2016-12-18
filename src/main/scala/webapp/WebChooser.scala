package webapp

import implementationSearcher._
import shared.{BigOLiteral, ConstantTime}

import scala.scalajs.js.annotation.JSExport
import scala.util.Try
import upickle.default


import scala.scalajs.js
import js.JSConverters._

/**
  * Created by buck on 12/17/16.
  */
@JSExport
case class WebChooser(@JSExport library: ImplLibrary, text: LibraryText) {
  @JSExport
  def implsString: String = default.write(library.impls.map(
    (x) => x.name.name -> x.lhs).toMap)

  @JSExport
  def structuresString: String = default.write(
    library.structures.mapValues(_.parameters))

  @JSExport
  def declsString: String = default.write(library.decls.map(
    { case (m: MethodName, i: ImplDeclaration) => m.name -> i}
  ))

  @JSExport
  def dataStructureCommentsString: String = default.write(text.dataStructureComments)

  @JSExport
  def choose(methods: js.Array[String]): js.Array[DataStructureChoice] = {
    (for {
      adt <- Try(AbstractDataType(Map(),
        methods.map((name: String) => {
          MethodExpr(name) -> (ConstantTime: BigOLiteral)
        }).toMap))

      searchResults <- Try(
        Searcher.allParetoOptimalDataStructureCombosForAdt(library, adt).items)
    } yield searchResults).get.toJSArray
  }
}

object WebChooser {
  def build(implsString: String, structuresString: Set[String]): Try[WebChooser] = {
    ImplLibrary.buildFromStrings(implsString, structuresString).map({
      case (library: ImplLibrary, text: LibraryText) => WebChooser(library, text)
    })
  }
}
