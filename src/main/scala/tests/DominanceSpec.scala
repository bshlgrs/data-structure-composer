package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec
import shared._
import org.scalatest.prop.Checkers
//import org.scalacheck.Arbitrary._
//import org.scalacheck.Prop._


class DominanceSpec extends FunSpec with Checkers {
  describe("Affine combos") {
    describe ("when they have no weights") {
      def Rhs(bias: BigOLiteral, weights: Map[String, BigOLiteral] = Map()) = {
        AffineBigOCombo(bias, weights)
      }

      it("handles them being equal") {
        assert(Rhs(ConstantTime).partialCompare(Rhs(ConstantTime)) == BothDominate)
      }

      it("handles the LHS being greater") {
        assert(Rhs(LogTime).partialCompare(Rhs(ConstantTime)) == RightStrictlyDominates)
      }

      it("handles the RHS being greater") {
        assert(Rhs(ConstantTime).partialCompare(Rhs(LogTime)) == LeftStrictlyDominates)
      }
    }

//    it ("also lists work") {
//      check((a: List[Int], b: List[Int]) => a.size + b.size == (a ::: b).size)
//    }
  }

  describe("ImplLhs") {
    val partialCompare = ImplLhs.ImplLhsPartialOrdering.partialCompare _
    it("knows that both things dominate if equal") {
      assert(partialCompare(ImplLhs.parse("foo"), ImplLhs.parse("foo"))
        == BothDominate)

      assert(partialCompare(ImplLhs.parse("foo[x]"), ImplLhs.parse("foo[x]"))
        == BothDominate)

      assert(partialCompare(ImplLhs.parse("foo[x]"), ImplLhs.parse("foo[y]"))
        == BothDominate)
    }

    // A dominates B if A can be used to implement B
    it("knows that one dominates another") {
      assert(partialCompare(ImplLhs.parse("foo[x]"), ImplLhs.parse("foo[x] if x.bar"))
        == LeftStrictlyDominates)

      assert(partialCompare(ImplLhs.parse("foo[x] if x.bar"), ImplLhs.parse("foo[x]"))
        == RightStrictlyDominates)
    }

    it("knows that both dominate") {
      assert(partialCompare(ImplLhs.parse("foo[x] if x.bar"), ImplLhs.parse("foo[x] if x.baz"))
        == NeitherDominates)
    }
  }

  describe("Impls") {
    val partialCompare = Impl.ImplPartialOrdering.partialCompare _
    it("compares runtimes") {
      assert(partialCompare(Impl("foo <- n"), Impl("foo <- log(n)"))
        == RightStrictlyDominates)
    }

    it("compares conditions") {
      assert(partialCompare(
        Impl("reduce[f] <- n + n * f"),
        Impl("reduce[f] if f.commutative <- n + n * f"))
        == LeftStrictlyDominates)
    }

    it("deals with conditions and runtimes conflicting") {
      assert(partialCompare(
        Impl("reduce[f] <- n + n * f"),
        Impl("reduce[f] if f.commutative <- log(n)"))
        == NeitherDominates)
    }
  }
}
