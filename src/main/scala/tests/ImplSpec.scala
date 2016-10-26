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
    val emptySearchResults = UnfreeImplSet(Map(), Set(), Map(MethodName("f") -> ImplDeclaration(Nil)))

    it("does simple constant time test") {
      val impl = Impl("f <- 1")
      val Some((conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f"), emptySearchResults).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("1"))
    }

    it("does simple linear time test") {
      val impl = Impl("f <- n")
      val Some((conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f"), emptySearchResults).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n"))
    }

    it("correctly passes parameters through") {
      val impl = Impl("f[x] <- n * x")
      val Some((conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f[y]"), Scope(Map("y" -> Set()), emptySearchResults)).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n * y"))
    }

    it("correctly deals with inapplicable anonymous functions") {
      val impl = Impl("f[x] <- x")
      val Some((conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f[_]"), emptySearchResults).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("1"))
    }

    it("correctly deals with applicable anonymous functions") {
      val impl = Impl("f[x] if x.foo <- x")
      val Some((conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("f[_{foo}]"), emptySearchResults).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("1"))
    }

    it("returns sums") {
      val impl = Impl("f[x] <- x + log(n)")
      val Some((conditions, rhs)) =
        impl.bindToContext(MethodExpr.parse("g[y]"), Scope(Map("y" -> Set()), emptySearchResults)).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("y + log(n)"))
    }

    it("notices when anonymous functions don't match the impl conditions") {
      val impl = Impl("f[x] if x.foo <- x")
      val res = impl.bindToContext(MethodExpr.parse("f[_]"), emptySearchResults).headOption

      assert(res.isEmpty)
    }

    it("reports impl conditions for named args") {
      val impl = Impl("f[x] if x.foo <- x")
      val Some((conditions, rhs)) = impl.bindToContext(MethodExpr.parse("f[y]"), Scope(Map("y" -> Set()), emptySearchResults)).headOption

      assert(conditions == ImplPredicateMap(Map("y" -> Set("foo"))))
      assert(rhs == Impl.rhs("y"))
    }

    it ("doesn't screw up an obvious thing (regression test)") {
      val impl = Impl("getFirstBy[f]  <- n * f + n")
      val Some((conditions, rhs)) = impl.bindToContext(MethodExpr.parse("getFirstBy[y]"), Scope(Map("y" -> Set()), emptySearchResults)).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n * y + n"))
    }

    it("handles anonymous functions from underscore") {
      val impl = Impl("y[f] <- n * f")
      val Some((conditions, rhs)) = impl.bindToContext(
        MethodExpr.parse("y[_ <- k]"),
        Scope(Map("y" -> Set()), ImplSet.fromSetOfImpls(Set(Impl("k <- 1"))))
      ).headOption

      assert(conditions.isEmpty)
      assert(rhs == Impl.rhs("n"))
    }
  }
}
