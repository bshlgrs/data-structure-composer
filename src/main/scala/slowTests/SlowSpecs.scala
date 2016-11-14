package slowTests

/**
  * Created by buck on 9/12/16.
  */

import cli.DataStructureChooserCli
import finatraServer.{SearchController, SearchRequest}
import implementationSearcher._
import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class SlowSpecs extends FunSpec with GeneratorDrivenPropertyChecks {
  val controller = new SearchController


  val allMethodExprs = DataStructureChooserCli.decls.map({ case (name, decl) =>
    MethodExpr(name, decl.parameters.map((x) => UnderscoreFunctionExpr))
  })

  describe("random tests") {
    it("works") {
      forAll (Gen.someOf(allMethodExprs)) { (exprs) =>
        println(exprs)

        val searchResult = controller.search(SearchRequest(None, None, exprs.map(_.toString).toSet)).get

        searchResult.items.foreach(_.frontendResult)
      }
    }
  }

  describe("slow, methodical tests") {
    it("works") {
      allMethodExprs.foreach((m) => {
        val searchResult = controller.search(SearchRequest(None, None, Set(m.toString, "getByIndex"))).get

        assert(searchResult.items.nonEmpty, s"no implementation for $m")
        searchResult.items.foreach(_.frontendResult)
      })
    }
  }
}
