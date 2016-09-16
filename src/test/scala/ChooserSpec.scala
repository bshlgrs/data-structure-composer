/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec
import shared.{ConstantTime, LogTime}

class ChooserSpec extends FunSpec {

  describe("Search") {
//    it("correctly infers times in the simple case") {
//    }

    it("correctly infers conditions") {
      val yLhs = ImplLhs("y", List("g"))
      val yRhs = ImplRhs(ConstantTime, Map(MethodExpr("x", List(NamedFunctionExpr("g"))) -> ConstantTime))
      val y = Impl(yLhs, yRhs, None)
      val testLibrary = Set(
//        Impl(ImplLhs("x", List("f"), Some(ImplPredicateList(List(Set("foo"))))), ImplRhs(LogTime))
        y
      )

      val res = Chooser.getAllTimes(testLibrary)
    }
  }

  // x[f] if x.foo <- 1
  // y[g] <- x[g]
  // should infer
  // y[g] if g.foo <- 1

}
