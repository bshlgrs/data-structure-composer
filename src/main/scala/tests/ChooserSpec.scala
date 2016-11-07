package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec

class ChooserSpec extends FunSpec {
  describe("Search") {
    it("does simple things") {
      val (impls, decls) = ImplDeclaration.parseMany(
        "x <- n",
        "y <- x",
        "z <- y"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      assert(res.getNamedWithoutSource("x") == Set(Impl("x <- n")))
      assert(res.getNamedWithoutSource("y") == Set(Impl("y <- n")))
      assert(res.getNamedWithoutSource("z") == Set(Impl("z <- n")))
    }

    it("does simple parameterized things") {
      val (impls, decls) = ImplDeclaration.parseMany(
        "x[f] <- n * f",
        "y <- x[_]"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      assert(res.getNamedWithoutSource("y") == Set(Impl("y <- n")))
    }

    it("does more complex parameterized things") {
      val (impls, decls) = ImplDeclaration.parseMany(
        "x[g] <- n * g",
        "y[f] <- x[f]"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      val expected = Impl(ImplLhs("y", ImplPredicateMap(Map(MethodName("f") -> Set()))), Impl.rhs("n * f"))
      assert(res.getNamedWithoutSource("y") == Set(expected))
    }

    it("correctly infers conditions") {
      val (impls, decls) = ImplDeclaration.parseMany(
        "x[f] if f.foo <- log(n) + f",
        "y[g] <- x[g]"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      assert(res.getNamedWithoutSource("y").head == Impl("y[g] if g.foo <- log(n) + g"))
    }

    it("doesn't use methods without necessary conditions") {
      val (impls, decls) = ImplDeclaration.parseMany(
        "x[f] if f.foo <- n",
        "y <- x[_]"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      assert(res.getNamed("y").isEmpty)
    }

    it("handles named functions in method expressions") {
      val (impls, decls) = ImplDeclaration.parseMany(
        "y[f] <- n * f",
        "x <- y[k]",
        "k <- 1"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      assert(res.getNamedWithoutSource(MethodName("x")) == Set(Impl("x <- n")))
    }

    it("handles anonymous functions from underscore") {
      val weirdAssImpl = Impl("x <- y[_ <- k]")

      assert(weirdAssImpl.getNames == Set[MethodName]("y", "k"))

      val (impls, decls) = ImplDeclaration.parseMany(
        "y[f] <- n * f",
        weirdAssImpl.toString(),
        "k <- 1"
      )

      val res = Chooser.getAllTimesFromEmpty(impls, ImplLibrary(impls, decls, Map()), Set())

      assert(res.getNamedWithoutSource(MethodName("x")) == Set(Impl("x <- n")))
    }
  }

}
