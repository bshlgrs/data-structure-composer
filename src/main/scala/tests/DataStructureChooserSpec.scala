package tests

/**
  * Created by buck on 9/12/16.
  */

import cli.DataStructureChooserCli
import implementationSearcher._
import org.scalatest.FunSpec
import parsers.MainParser
import shared._

class DataStructureChooserSpec extends FunSpec {
  val impls = DataStructureChooserCli.impls
  val decls = DataStructureChooserCli.decls

  val linkedList = DataStructure(
    """ds LinkedList {
      |    getFirst <- 1
      |    getNext <- 1
      |    updateNode! <- 1
      |    insertAfterNode! <- 1
      |    insertFirst! <- 1
      |}""".stripMargin, decls)

  val genericHeap = DataStructure(
    """ds GenericHeap[g] {
      |    updateNode! <- log(n) + g
      |    getFirstBy[f] <- 1
      |    insertAtIndex! <- log(n) + g
      |    unorderedEach[f] <- n + n * f
      |}""".stripMargin, decls)


  val heap = DataStructure(
    """ds Heap {
      |    getMinimum <- 1
      |    updateNode! <- log(n)
      |    insertAtIndex! <- log(n)
      |    unorderedEach[f] <- n + n * f
      |}""".stripMargin, decls)


  val stackMemoizer = DataStructure(
    """ds StackMemoizer[reduction] if reduction.idempotent {
      |    insertLast! <- reduction
      |    deleteLast! <- reduction
      |    reduce[reduction, zero] <- 1
      |}""".stripMargin, decls)

  val library: ImplLibrary = ImplLibrary(impls, decls,
    Map("GenericHeap" -> genericHeap, "LinkedList" -> linkedList))


  describe("data structure analysis") {
    describe("integration tests") {
      it("can do a linked list") {
        val res = Chooser.getAllTimesForDataStructure(library, linkedList)

        assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- n")))
        assert(res.getNamed("getNext") == Set(Impl("getNext <- 1")))
        assert(res.getNamed("updateNode!") == Set(Impl("updateNode! <- 1")))
      }

      it("can do a heap") {
        val res = Chooser.getAllTimesForDataStructure(library, heap)

        assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
        assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- 1")))
        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- n")))
        assert(res.getNamed("updateNode!") == Set(Impl("updateNode! <- log(n)")))
      }

      it("can do linked-list + heap") {
        val res =
          Chooser.getRelevantTimesForDataStructures(library, Set(linkedList, heap))

        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamed("insertAnywhere!") == Set(Impl("insertAnywhere! <- log(n)")))
        assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- 1")))
      }

      describe("doing a generic heap") {
        val res = Chooser.getAllTimesForDataStructure(library, genericHeap)

        it("succeeds at the read methods") {
          assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
          assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- 1")))
          assert(res.getNamed("getFirst") == Set(Impl("getFirst <- n")))
          assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
          assert(res.getNamed("getNext") == Set(Impl("getNext <- n")))
        }

        it("succeeds at the write methods") {
          assert(res.getNamed("updateNode!") == Set(Impl("updateNode! <- g + log(n)")))
        }
      }

      it("can do linked-list + generic heap") {
        val res = Chooser.getRelevantTimesForDataStructures(library, Set(linkedList, genericHeap))

        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamed("insertAnywhere!") == Set(Impl("insertAnywhere! <- g + log(n)")))
        assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- 1")))
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

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(library, listAdt)

        assert(res.items == Set(linkedListResult))
      }

      it("can choose the best options for a List adt") {
        val listAdt = AbstractDataType(Map(), Map(MethodExpr.parse("getFirst") -> ConstantTime, MethodExpr.parse("getNext") -> ConstantTime))

        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, listAdt)

        assert(res.items == Set(linkedListResult))
      }
    }

    describe("priority queue ADT") {
      val linkedListPQResult = DataStructureChoice(
        Set("LinkedList"),
        Map(MethodExpr.parse("getMinimum") -> AffineBigOCombo(LinearTime, Map()),
          MethodExpr.parse("insertAnywhere!") -> AffineBigOCombo(ConstantTime, Map())))

      val heapResult = DataStructureChoice(
        Set("GenericHeap"),
        Map(MethodExpr.parse("getMinimum") -> AffineBigOCombo(ConstantTime, Map()),
          MethodExpr.parse("insertAnywhere!") -> Impl.rhs("g + log(n)").mapKeys(_.getAsNakedName)))

      it("can choose the Pareto-optimal options for a PriorityQueue adt") {
        val pQueueAdt = AbstractDataType(Map(),
          Map(MethodExpr.parse("getMinimum") -> ConstantTime,
            MethodExpr.parse("insertAnywhere!") -> ConstantTime)
        )

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(library, pQueueAdt)

        assert(res.items == Set(linkedListPQResult, heapResult))
      }

      it("can choose the best options for a PriorityQueue adt") {
        val pQueueAdt = AbstractDataType(Map(),
          Map(MethodExpr.parse("getMinimum") -> ConstantTime,
            MethodExpr.parse("insertAnywhere!") -> ConstantTime)
        )

        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, pQueueAdt)

        assert(res.items == Set(heapResult))
      }
    }
  }
}
