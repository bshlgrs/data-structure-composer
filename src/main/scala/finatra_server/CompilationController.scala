package finatra_server

import cli.DataStructureChooserCli
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import implementationSearcher.{ImplLhs, MethodExpr}

import scala.util.{Failure, Success, Try}
import parsers.MainParser

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

//  get("/search") search{ searchRequest: SearchRequest =>
//    val result = for {
//      (impls, decls) <- searchRequest.mbImplsString match {
//        MainParser
//      }.parseImplFileString(searchRequest.`mbImplsString`)
//      dataStructures <- MainParser.parseDataStructureFileString(
//        searchRequest.dataStructuresString, decls)
//
//    }
//  }

  get("/") { request: Request =>
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
