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

    it("does parameterized things") {
      val testLibrary = Set(
        Impl("x[f] <- n * f"),
        Impl("y <- x[_]")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.get("y") == Set(UnfreeImpl("y <- n")))
    }

    it("correctly infers conditions") {
      val testLibrary = Set(
        Impl("x[f] if f.foo <- log(n) + f"),
        Impl("y[g] <- x[g]")
      )

      val res = Chooser.getAllTimes(testLibrary)

      assert(res.impls(MethodName("y")).options.head == UnfreeImpl("y[g] if g.foo <- log(n) + g"))
    }
  }
//   x[f] if x.foo <- 1
//   y[g] <- x[g]
//   should infer
//   y[g] if g.foo <- 1
}
