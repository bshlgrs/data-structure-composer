package tests

/**
  * Created by buck on 9/12/16.
  */

import implementationSearcher._
import org.scalatest.FunSpec
import shared._

class DataStructureChooserSpec extends FunSpec {
  val impls = Set(
    Impl("getByIndex <- getFirst + n * getNext"),
    Impl("getByIndex <- unorderedEach[_]"),
    Impl("getFirst <- getByIndex"),
    Impl("getNext <- getByIndex"),
    Impl("unorderedEach[f] <- getFirst + n * getNext + n * f"),
    Impl("getSmallest <- getSmallestBy[valueOrdering]"),
    Impl("getSmallestBy[f] <- unorderedEach[_ <- f]"),
    Impl("insertAtIndex! <- getByIndex + insertAfterNode!"),
    Impl("insertAnywhere! <- insertAfterNode! + getFirst"),
    Impl("insertAnywhere! <- insertAtIndex!"),
    Impl("valueOrdering <- 1")
  )

  val linkedList = DataStructure(
    """ds LinkedList {
      |    getFirst <- 1
      |    getNext <- 1
      |    updateNode! <- 1
      |    insertAfterNode! <- 1
      |}""".stripMargin)

  val heap = DataStructure(
    """ds Heap {
      |    getSmallest <- 1
      |    updateNode! <- log(n)
      |    insertAtIndex! <- log(n)
      |    unorderedEach[f] <- n + n * f
      |}""".stripMargin)

  val genericHeap = DataStructure(
    """ds Heap[f] {
      |    getSmallestBy[f] <- 1
      |    updateNode! <- log(n)
      |    insertAtIndex! <- log(n)
      |    unorderedEach[g] <- n + n * g
      |}""".stripMargin)


  describe("data structure analysis") {
    describe("integration tests") {
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
        assert(res.get("getByIndex") == Set(UnfreeImpl("getByIndex <- n")))
        assert(res.get("getNext") == Set(UnfreeImpl("getNext <- n")))
      }

      it("can do linked-list + generic heap") {
        val res = Chooser.getRelevantTimesForDataStructures(impls, Set(linkedList, genericHeap))

        assert(res.get("getFirst") == Set(UnfreeImpl("getFirst <- 1")))
        assert(res.get("insertAnywhere!") == Set(UnfreeImpl("insertAnywhere! <- log(n)")))
        assert(res.get("getSmallest") == Set(UnfreeImpl("getSmallest <- 1")))
      }
    }
  }

  describe("adt analysis") {
    describe("List adt") {
      val linkedListResult = DataStructureChoice(
        Set("LinkedList"),
        Map(MethodExpr.parse("getFirst") -> AffineBigOCombo(ConstantTime, Map()),
          MethodExpr.parse("getNext") -> AffineBigOCombo(ConstantTime, Map())))

      it("can choose the Pareto-optimal options for a List adt") {
        val listAdt = AbstractDataType(Map(), Map(MethodExpr.parse("getFirst") -> ConstantTime, MethodExpr.parse("getNext") -> ConstantTime))

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(impls, Set(linkedList, genericHeap), listAdt)

        assert(res.items == Set(linkedListResult))
      }

      it("can choose the best options for a List adt") {
        val listAdt = AbstractDataType(Map(), Map(MethodExpr.parse("getFirst") -> ConstantTime, MethodExpr.parse("getNext") -> ConstantTime))

        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(impls, Set(linkedList, genericHeap), listAdt)

        assert(res.items == Set(linkedListResult))
      }
    }

    describe("priority queue ADT") {
      val linkedListPQResult = DataStructureChoice(
        Set("LinkedList"),
        Map(MethodExpr.parse("getSmallest") -> AffineBigOCombo(LinearTime, Map()),
          MethodExpr.parse("insertAnywhere!") -> AffineBigOCombo(ConstantTime, Map())))

      val heapResult = DataStructureChoice(
        Set("Heap"),
        Map(MethodExpr.parse("getSmallest") -> AffineBigOCombo(ConstantTime, Map()),
          MethodExpr.parse("insertAnywhere!") -> AffineBigOCombo(LogTime, Map())))

      it("can choose the Pareto-optimal options for a PriorityQueue adt") {
        val pQueueAdt = AbstractDataType(Map(),
          Map(MethodExpr.parse("getSmallest") -> ConstantTime,
            MethodExpr.parse("insertAnywhere!") -> ConstantTime)
        )

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(impls, Set(linkedList, genericHeap), pQueueAdt)

        assert(res.items == Set(linkedListPQResult, heapResult))
      }

      it("can choose the best options for a PriorityQueue adt") {
        val pQueueAdt = AbstractDataType(Map(),
          Map(MethodExpr.parse("getSmallest") -> ConstantTime,
            MethodExpr.parse("insertAnywhere!") -> ConstantTime)
        )

        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(impls, Set(linkedList, genericHeap), pQueueAdt)

        assert(res.items == Set(heapResult))
      }
    }
  }
}
