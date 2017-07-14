package finatraServer

import cli.DataStructureChooserCli
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import implementationSearcher._

import scala.util.{Failure, Success, Try}
import parsers.MainParser
import shared.{BigOLiteral, ConstantTime, DominanceFrontier}

import scala.collection.mutable

class SearchController extends Controller {
  // todo: make this an LRU cache
  val cache: mutable.Map[SearchRequest, Try[DominanceFrontier[DataStructureChoice]]] = {
    mutable.Map()
  }

  def search(searchRequest: SearchRequest): Try[DominanceFrontier[DataStructureChoice]] = {
    println(searchRequest)
    for {
      (impls, decls) <- (searchRequest.mbImplsString match {
        case None => Success(DataStructureChooserCli.impls, DataStructureChooserCli.decls)
        case Some(text) => MainParser.parseImplFileString(text)
      }): Try[(Set[FreeImpl], ImplLibrary.Decls)]
      dataStructures <- searchRequest.dataStructuresString match {
        case None => Success(DataStructureChooserCli.dataStructures)
        case Some(text) => MainParser.parseDataStructureFileString(text, decls)
      }
      adt <- Try(AbstractDataType(Map(),
        searchRequest.adtMethods.map((x: String) => {
//          val methodName = MethodName(x)
//          val args = List.fill(decls(methodName).parameters.length)(UnderscoreFunctionExpr: FunctionExpr)

          MethodExpr.parse(x) -> (ConstantTime: BigOLiteral)
        }).toMap))

      library = ImplLibrary(impls, decls, dataStructures)
      _ = println("Everything was parsed correctly")
      searchResults <- Try(
        Searcher.allParetoOptimalDataStructureCombosForAdt(library, adt))
      _ = println("Search happened correctly")
      _ <- Try(searchResults.items.map(_.frontendResult.get))
    } yield searchResults
  }


  post("/search") { (searchRequest: SearchRequest) => {
    val caching = true

    val resultTry: Try[DominanceFrontier[DataStructureChoice]] = if (caching) {
      if (cache.contains(searchRequest)) {
        cache(searchRequest)
      } else {
        val computedResult = search(searchRequest)
        cache(searchRequest) = computedResult
        computedResult
      }
    } else {
      search(searchRequest)
    }

    resultTry match {
      case Success(result) => {
//        println(result.items.map(_.frontendResult))
        response.ok.body(result.items)
      }
      case Failure(err) => response.badRequest(err)
    }
  }}

  get("/start-data") { request: Request =>
//    response.ok.body(ImplLhs.parse("f[y] if y.foo"))
    response.ok.body(
      Map(
        "implText" -> DataStructureChooserCli.libraryText,
        "impls" -> DataStructureChooserCli.impls,
        "decls" -> DataStructureChooserCli.decls,
        "dataStructures" -> DataStructureChooserCli.dataStructures,
        "dataStructureTexts" -> DataStructureChooserCli.dataStructureTexts,
        "publicMethods" -> DataStructureChooserCli.publicMethods.map(_.name)
      )
    )
  }

  get("/:*") { request: Request =>
    response.ok.file(
      request.params("*"))
  }

  get("/random-questions") { request: Request =>
    DataStructureChooserCli.randomMethods()
  }
}
