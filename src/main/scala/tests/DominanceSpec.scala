package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec
import shared._

class DominanceSpec extends FunSpec {
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

  describe("Unfree impls") {
    val partialCompare = UnfreeImpl.UnfreeImplPartialOrdering.partialCompare _
    it("does simple things") {
      assert(partialCompare(UnfreeImpl("foo <- n"), UnfreeImpl("foo <- log(n)"))
        == RightStrictlyDominates)
    }
  }
}
