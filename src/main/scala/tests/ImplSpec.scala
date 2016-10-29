package tests

import implementationSearcher._
import org.scalatest.FunSpec


/**
  * Created by buck on 9/18/16.
  */
class ImplSpec extends FunSpec {

  describe("bindToContext") {
    //    it("correctly infers times in the simple case") {
    //    }

    val emptyLhs = ImplLhs.parse("whatever")
    val basicUnfreeImplSet = UnfreeImplSet(
      Map(),
      Set(),
      Map(
        MethodName("f") -> ImplDeclaration(List("x")),
        MethodName("getFirstBy") -> ImplDeclaration(List("f")),
        MethodName("y") -> ImplDeclaration(List("f")),
        MethodName("g") -> ImplDeclaration(Nil)
      ))

    it("does simple constant time test") {
      val impl = Impl("g <- 1")
      val Some(UnnamedImpl(conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("g"), basicUnfreeImplSet, ParameterList.empty).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("1"))
    }

    it("does simple linear time test") {
      val impl = Impl("g <- n")
      val Some(UnnamedImpl(conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("g"), basicUnfreeImplSet, ParameterList.empty).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n"))
    }

    it("correctly passes parameters through") {
      val impl = Impl("f[x] <- n * x")
      val Some(UnnamedImpl(conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f[y]"), basicUnfreeImplSet, ParameterList.easy("y")).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n * y"))
    }

    it("correctly deals with inapplicable anonymous functions") {
      val impl = Impl("f[x] <- x")
      val Some(UnnamedImpl(conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f[_]"), basicUnfreeImplSet, ParameterList.empty).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n"))
    }

    it("correctly deals with applicable anonymous functions") {
      val impl = Impl("f[x] if x.foo <- x")
      val Some(UnnamedImpl(conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f[_{foo}]"), basicUnfreeImplSet, ParameterList.empty).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n"))
    }

    it("returns sums") {
      val impl = Impl("f[x] <- x + log(n)")
      val Some(UnnamedImpl(conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("g[y]"), basicUnfreeImplSet, ParameterList.easy("y")).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("y + log(n)"))
    }

    it("notices when anonymous functions don't match the impl conditions") {
      val impl = Impl("f[x] if x.foo <- x")
      val res = impl.bindToContext(MethodExpr.parse("f[_]"), basicUnfreeImplSet, ParameterList.empty).headOption

      assert(res.isEmpty)
    }

    it("reports impl conditions for named args") {
      val impl = Impl("f[x] if x.foo <- x")
      val Some(UnnamedImpl(conditions, rhs)) = impl.bindToContext(MethodExpr.parse("f[y]"), basicUnfreeImplSet, ParameterList.easy("y")).headOption

      assert(conditions == ImplPredicateMap(Map(MethodName("y") -> Set("foo"))))
      assert(rhs == Impl.rhs("y"))
    }

    it ("doesn't screw up an obvious thing (regression test)") {
      val impl = Impl("getFirstBy[f]  <- n * f + n")
      val Some(UnnamedImpl(conditions, rhs)) = impl.bindToContext(MethodExpr.parse("getFirstBy[y]"), basicUnfreeImplSet, ParameterList.easy("y")).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n * y + n"))
    }

    it("handles anonymous functions from underscore") {
      val impl = Impl("y[f] <- n * f")
      val Some(UnnamedImpl(conditions, rhs)) = impl.bindToContext(
        MethodExpr.parse("y[_ <- k]"),
        basicUnfreeImplSet,
        ParameterList.easy("y")
      ).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n"))
    }
  }
}
