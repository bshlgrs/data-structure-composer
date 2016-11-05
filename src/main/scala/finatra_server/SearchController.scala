package finatra_server

import cli.DataStructureChooserCli
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import implementationSearcher._

import scala.util.{Failure, Success, Try}
import parsers.MainParser
import shared.{BigOLiteral, ConstantTime, DominanceFrontier}

class SearchController extends Controller {
//  get("/compile") { compilationRequest: CompilationRequest =>
//    val result = for {
//      javaClass <- Try(JavaParserWrapper.parseJavaClassToAst(compilationRequest.contents))
//      querified <- Try(javaClass.querify())
//      auxiliaryDataStructures <- Try(querified.queries().map({ (x) =>
//        x -> UnorderedDataStructureLibrary.getBestStructureForClass(x, querified)
//      }).toMap)
//      optimizedClass <- if (compilationRequest.optimization)
//        Try(querified.actualizeQueries(auxiliaryDataStructures))
//      else
//        Try(querified.actualizeQueries(Map()))
//      out <- Try(RubyOutputter.outputClass(optimizedClass))
//    } yield out
//
//    result match {
//      case Success(out) => out
//      case Failure(exception) => exception.toString ++ "\n\n" ++ exception.getStackTrace.map(_.toString).mkString("\n")
//    }
//  }

  post("/search") { searchRequest: SearchRequest =>
    val tryResult: Try[DominanceFrontier[DataStructureChoice]] = for {
      (impls, decls) <- (searchRequest.mbImplsString match {
        case None => Success(DataStructureChooserCli.impls, DataStructureChooserCli.decls)
        case Some(text) => MainParser.parseImplFileString(text)
      }) : Try[(Set[Impl], ImplLibrary.Decls)]
      dataStructures <- searchRequest.dataStructuresString match {
        case None => Success(DataStructureChooserCli.dataStructures)
        case Some(text) => MainParser.parseDataStructureFileString(text, decls)
      }
      adt <- Try(AbstractDataType(Map(),
        searchRequest.adtMethods.map((x: String) =>
          MethodExpr.parse(x) -> (ConstantTime: BigOLiteral)).toMap))
      library <- Try(ImplLibrary(impls, decls, dataStructures))
      searchResults <- Try(Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, adt))
    } yield searchResults

    tryResult match {
      case Success(result) => response.ok.body(result.items)
      case Failure(err) => response.badRequest(err)
    }
  }

  get("/start-data") { request: Request =>
//    response.ok.body(ImplLhs.parse("f[y] if y.foo"))
    response.ok.body(
      Map(
        "implText" -> DataStructureChooserCli.libraryText,
        "impls" -> DataStructureChooserCli.impls,
        "decls" -> DataStructureChooserCli.decls,
        "dataStructures" -> DataStructureChooserCli.dataStructures,
        "dataStructureText" -> DataStructureChooserCli.dataStructuresText
      )
    )
  }

  get("/:*") { request: Request =>
    response.ok.file(
      request.params("*"))
  }
}
