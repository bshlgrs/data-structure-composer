package webapp

import implementationSearcher._
import parsers.MainParser

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document

import scala.scalajs.js.annotation.JSExport

/**
  * Created by buck on 9/15/16.
  */
object WebApp extends JSApp {
  def main(): Unit = {

  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  @JSExport
  def makeChoices(implString: String, dsString: String, adtString: String): js.Array[js.Dictionary[js.Any]] = {
    val (impls, decls) = MainParser.parseImplFileString(implString).get
    val structures = MainParser.parseDataStructureFileString(dsString, decls).get
    val library = ImplLibrary(impls, decls, structures)
    val adt = AbstractDataType.parse(adtString)
    val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, adt)

    js.Array(res.items.map((choice) => {
      js.Dictionary[js.Any](
        "choices" -> js.Array(choice.choices.toArray:_*),
        "times" -> js.Dictionary(choice.results.map({case (m, cost) =>
          m.toString -> cost.toString
        }).toSeq:_*)
      )
    }).toList: _*)
  }
}
