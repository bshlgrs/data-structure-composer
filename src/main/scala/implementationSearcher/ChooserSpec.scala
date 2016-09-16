package implementationSearcher

/**
  * Created by buck on 9/12/16.
  */

import org.scalatest.FunSpec
import shared.{LogTime, ConstantTime}

class ChooserSpec extends FunSpec {

  describe("Search") {
//    it("correctly infers times in the simple case") {
//    }

    it("correctly infers conditions") {
      val testLibrary = Set(
        Impl(ImplLhs("x", List("f"), Some(ImplPredicateList(List(Set("foo"))))), ImplRhs("log(n)")),
        Impl(ImplLhs("y", List("g")), ImplRhs(ConstantTime, Map(MethodExpr("x", List(NamedFunctionExpr("g"))) -> ConstantTime)), None)
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.impls(MethodName("y")).options.head ==
        UnfreeImpl(ImplLhs("y", List("g"), Some(ImplPredicateList(List(Set("foo"))))), ImplRhs(LogTime)))
    }
  }

  // x[f] if x.foo <- 1
  // y[g] <- x[g]
  // should infer
  // y[g] if g.foo <- 1

}
