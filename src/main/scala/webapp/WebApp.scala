package webapp

import implementationSearcher.{DataStructure, Chooser, Impl}
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
    appendPar(document.body, "Hello World")
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  @JSExport
  def makeChoices(implsString: String, dataStructuresString: String): js.Array[js.Array[String]] = {
//    val impls: Set[Impl] = ??? // MainParser.impls.parse(implsString).get.value
//    val dataStructures: Set[DataStructure] = MainParser.dataStructureFile.parse(dataStructuresString).get.value
//
//    val resultStrings = {
//      dataStructures
//        .map((x) =>
//          x -> Chooser.getAllTimesForDataStructure(impls, x).toLongString)
//        .toList
//        .sortBy(_._1.name)
//        .map((x) => x._1.name -> x._2)
//        .map((x) => js.Array(x._1, x._2))
//    }
//
//    js.Array(resultStrings:_*)
    ???
  }
}
