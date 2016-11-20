package webapp

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
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

  //  @JSExport
//  def makeChoices(implString: String, dsString: String, adtString: String): js.Array[js.Dictionary[js.Any]] = {
//    val (impls, decls) = MainParser.parseImplFileString(implString).get
//    val structures = MainParser.parseDataStructureFileString(dsString, decls).get
//    val library = ImplLibrary(impls, decls, structures)
//    val adt = AbstractDataType.parse(adtString)
//    val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, adt)
//
//    js.Array(res.items.map((choice) => {
//      js.Dictionary[js.Any](
//        "choices" -> js.Array(choice.choices.toArray:_*),
//        "times" -> js.Dictionary(choice.results.map({case (m, cost) =>
//          m.toString -> cost.toString
//        }).toSeq:_*)
//      )
//    }).toList: _*)
//  }
  @JSExport
  def makeInferencesFromMethods(inputs: String): UnfreeImplSet = {
    val (impls, decls) = MainParser.parseImplFileString(inputs).get
    val library = ImplLibrary(impls, decls, Map())

    Searcher.getAllTimes(UnfreeImplSet(Map(), Set()), impls, library)
  }
}
