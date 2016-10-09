package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec

class ChooserSpec extends FunSpec {
  describe("Search") {
    it("does simple things") {
      val testLibrary = Set(
        Impl("x <- n"),
        Impl("y <- x"),
        Impl("z <- y")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.get("x") == Set(UnfreeImpl("x <- n")))
      assert(res.get("y") == Set(UnfreeImpl("y <- n")))
      assert(res.get("z") == Set(UnfreeImpl("z <- n")))
    }

    it("does simple parameterized things") {
      val testLibrary = Set(
        Impl("x[f] <- n * f"),
        Impl("y <- x[_]")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.get("y") == Set(UnfreeImpl("y <- n")))
    }

    it("does more complex parameterized things") {
      val testLibrary = Set(
        Impl("x[g] <- n * g"),
        Impl("y[f] <- x[f]")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.get("y") == Set(UnfreeImpl("y[f] <- n * f")))
    }

    it("correctly infers conditions") {
      val testLibrary = Set(
        Impl("x[f] if f.foo <- log(n) + f"),
        Impl("y[g] <- x[g]")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.impls(MethodName("y")).impls.head == UnfreeImpl("y[g] if g.foo <- log(n) + g"))
    }

    it("handles named functions in method expressions") {
      val testLibrary = Set(
        Impl("y[f] <- n * f"),
        Impl("x <- y[k]"),
        Impl("k <- 1")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.impls(MethodName("x")).impls.head == UnfreeImpl("x <- n"))
    }

    it("handles anonymous functions from underscore") {
      val weirdAssImpl = Impl("x <- y[_ <- k]")

      println(weirdAssImpl.getNames)

      assert(weirdAssImpl.getNames == Set("y", "k"))

      val testLibrary = Set(
        Impl("y[f] <- n * f"),
        weirdAssImpl,
        Impl("k <- 1")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.impls(MethodName("x")).impls.head == UnfreeImpl("x <- n"))
    }
  }

}
