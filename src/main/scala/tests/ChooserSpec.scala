package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec

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

      println(res.toLongString)

      assert(res.impls(MethodName("y")).options.head == UnfreeImpl("y[g] if g.foo <- log(n) + g"))
    }

    it("handles named functions in method expressions") {
      val testLibrary = Set(
        Impl("y[f] <- n * f"),
        Impl("x <- y[k]"),
        Impl("k <- 1")
      )

      val res = Chooser.getAllTimes(testLibrary)

      println(res.toLongString)

      assert(res.impls(MethodName("x")).options.head == UnfreeImpl("x <- 1"))
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

      println(res.toLongString)

      assert(res.impls(MethodName("x")).options.head == UnfreeImpl("x <- n"))
    }
  }

  describe("data structure analysis") {
    val impls = Set(
      Impl("getByIndex <- getFirst + n * getNext"),
      Impl("getByIndex <- unorderedEach[_]"),
      Impl("getFirst <- getByIndex"),
      Impl("unorderedEach[f] <- getFirst + n * getNext + n * f"),
      Impl("getSmallest <- getSmallestBy[valueOrdering]"),
      Impl("getSmallestBy[f] <- unorderedEach[_ <- f]"),
      Impl("insertAtIndex! <- getByIndex + insertAfterNode!"),
      Impl("insertAnywhere! <- insertAfterNode! + getFirst"),
      Impl("insertAnywhere! <- insertAtIndex!"),
      Impl("valueOrdering <- 1")
    )

    val linkedList = DataStructure(
      """LinkedList {
        |    getFirst <- 1
        |    getNext <- 1
        |    updateNode! <- 1
        |    insertAfterNode! <- 1
        |}""".stripMargin)

    val heap = DataStructure(
      """Heap {
        |    getSmallest <- 1
        |    updateNode! <- log(n)
        |    insertAtIndex! <- log(n)
        |    unorderedEach[f] <- n + n * f
        |}""".stripMargin)

    val genericHeap = DataStructure(
      """Heap[f] {
        |    getSmallestBy[f] <- 1
        |    updateNode! <- log(n)
        |    insertAtIndex! <- log(n)
        |    unorderedEach[g] <- n + n * g
        |}""".stripMargin)


    it("can do a linked list") {
      val res = Chooser.getAllTimesForDataStructure(impls, linkedList)

      assert(res.get("getByIndex") == Set(UnfreeImpl("getByIndex <- n")))
      assert(res.get("getFirst") == Set(UnfreeImpl("getFirst <- 1")))
      assert(res.get("getSmallest") == Set(UnfreeImpl("getSmallest <- n")))
      assert(res.get("getNext") == Set(UnfreeImpl("getNext <- 1")))
      assert(res.get("updateNode!") == Set(UnfreeImpl("updateNode! <- 1")))
    }

    it("can do a heap") {
      val res = Chooser.getAllTimesForDataStructure(impls, heap)

      assert(res.get("getByIndex") == Set(UnfreeImpl("getByIndex <- n")))
      assert(res.get("getSmallest") == Set(UnfreeImpl("getSmallest <- 1")))
      assert(res.get("getFirst") == Set(UnfreeImpl("getFirst <- n")))
      assert(res.get("updateNode!") == Set(UnfreeImpl("updateNode! <- log(n)")))
    }

    it("can do linked-list + heap") {
      val res = Chooser.getRelevantTimesForDataStructures(impls, Set(linkedList, heap))

      assert(res.get("getFirst") == Set(UnfreeImpl("getFirst <- 1")))
      assert(res.get("insertAnywhere!") == Set(UnfreeImpl("insertAnywhere! <- log(n)")))
      assert(res.get("getSmallest") == Set(UnfreeImpl("getSmallest <- 1")))
    }

    it("can do a generic heap") {
      val res = Chooser.getAllTimesForDataStructure(impls, genericHeap)

      assert(res.get("getByIndex") == Set(UnfreeImpl("getByIndex <- n")))
      assert(res.get("getSmallest") == Set(UnfreeImpl("getSmallest <- 1")))
      assert(res.get("getFirst") == Set(UnfreeImpl("getFirst <- n")))
      assert(res.get("updateNode!") == Set(UnfreeImpl("updateNode! <- log(n)")))
    }
  }
}
