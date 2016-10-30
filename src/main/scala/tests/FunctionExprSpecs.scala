package tests

package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec
import shared._
import org.scalatest.prop.Checkers

class FunctionExprSpecs extends FunSpec {

  // I have f[x] <- x
  // I also have g[y] <- f[y]
  // I want to know that g[y] <- x, so I'm running getConditionsAndCosts on that y FunctionExpr in f[y]
  describe("getConditionsAndCosts") {
    it("works in a simple case") {
      val scope = UnfreeImplSet(Map(), Set(),
        Map(
          MethodName("f") -> ImplDeclaration(List("x")),
          MethodName("g") -> ImplDeclaration(List("y"))
        )
      )

      val conditionsAndCosts = NamedFunctionExpr("y").getConditionsAndCosts(Set(), scope,
        ParameterList.easy("y"))

      val expectedUnnamedImpl = UnnamedImpl(
        ImplPredicateMap(Map(MethodName("y") -> Set())),
        AffineBigOCombo[MethodExpr](ZeroTime,
          Map(MethodExpr.parse("y") -> ConstantTime)
        )
      )
      val expected = DominanceFrontier.fromSet(Set(expectedUnnamedImpl))

      assert(conditionsAndCosts == expected)
    }
  }
}
