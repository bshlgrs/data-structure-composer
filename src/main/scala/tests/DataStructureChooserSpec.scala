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
  val (impls, decls) = {
    val (impls1, decls1) = ImplDeclaration.parseMany(
      "getByIndex <- getFirst + n * getNext",
      "getByIndex <- unorderedEach[_]",
      "getFirst <- getByIndex",
      "getNext <- getByIndex",
      "unorderedEach[f] <- getFirst + n * getNext + n * f",
      "getSmallest <- getSmallestBy[valueOrdering]",
      "getSmallestBy[f] <- unorderedEach[_ <- f]",
      "insertAtIndex! <- getByIndex + insertAfterNode!",
      "insertAnywhere! <- insertAfterNode! + getFirst",
      "insertAnywhere! <- insertAtIndex!",
      "valueOrdering <- 1"
    )

    impls1 -> (decls1 ++ ImplDeclaration.parseManyFromLhses(
      "insertAfterNode!",
      "updateNode!"
    ))
  }

  val linkedList = DataStructure(
    """ds LinkedList {
      |    getFirst <- 1
      |    getNext <- 1
      |    updateNode! <- 1
      |    insertAfterNode! <- 1
      |}""".stripMargin)

  val vectorList = DataStructure(
    """ds VectorList {
      |    getByIndex <- 1
      |    updateNode! <- 1
      |    insertAtEnd! <- 1
      |    deleteAtIndex! <- n
      |    deleteBetweenNodes! <- n
      |    deleteLast! <- 1
      |}""".stripMargin)


  val genericHeap = DataStructure(
    """ds GenericHeap[g] {
      |    updateNode! <- log(n) + g
      |    getSmallestBy[f] <- 1
      |    insertAtIndex! <- log(n) + g
      |    unorderedEach[f] <- n + n * f
      |}""".stripMargin)


  val heap = DataStructure(
    """ds Heap {
      |    getSmallest <- 1
      |    updateNode! <- log(n)
      |    insertAtIndex! <- log(n)
      |    unorderedEach[f] <- n + n * f
      |}""".stripMargin)


  describe("data structure analysis") {
    describe("integration tests") {
      it("can do a linked list") {
        val res = Chooser.getAllTimesForDataStructure(impls, linkedList, decls)

        assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamed("getSmallest") == Set(Impl("getSmallest <- n")))
        assert(res.getNamed("getNext") == Set(Impl("getNext <- 1")))
        assert(res.getNamed("updateNode!") == Set(Impl("updateNode! <- 1")))
      }

      it("can do a heap") {
        val res = Chooser.getAllTimesForDataStructure(impls, heap, decls)

        assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
        assert(res.getNamed("getSmallest") == Set(Impl("getSmallest <- 1")))
        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- n")))
        assert(res.getNamed("updateNode!") == Set(Impl("updateNode! <- log(n)")))
      }

      it("can do linked-list + heap") {
        val res =
          Chooser.getRelevantTimesForDataStructures(impls, Set(linkedList, heap), decls)

        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamed("insertAnywhere!") == Set(Impl("insertAnywhere! <- log(n)")))
        assert(res.getNamed("getSmallest") == Set(Impl("getSmallest <- 1")))
      }

      describe("doing a generic heap") {
        val res = Chooser.getAllTimesForDataStructure(impls, genericHeap, decls)

        it("succeeds at the read methods") {
          assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
          assert(res.getNamed("getSmallest") == Set(Impl("getSmallest <- 1")))
          assert(res.getNamed("getFirst") == Set(Impl("getFirst <- n")))
          assert(res.getNamed("getByIndex") == Set(Impl("getByIndex <- n")))
          assert(res.getNamed("getNext") == Set(Impl("getNext <- n")))
        }

        it("succeeds at the write methods") {
          assert(res.getNamed("updateNode!") == Set(Impl("updateNode! <- g + log(n)")))
        }
      }

      it("can do linked-list + generic heap") {
        val res = Chooser.getRelevantTimesForDataStructures(impls, Set(linkedList, genericHeap), decls)

        assert(res.getNamed("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamed("insertAnywhere!") == Set(Impl("insertAnywhere! <- g + log(n)")))
        assert(res.getNamed("getSmallest") == Set(Impl("getSmallest <- 1")))
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

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(impls, Map("LinkedList" -> linkedList, "GenericHeap" -> genericHeap), decls, listAdt)

        assert(res.items == Set(linkedListResult))
      }

      it("can choose the best options for a List adt") {
        val listAdt = AbstractDataType(Map(), Map(MethodExpr.parse("getFirst") -> ConstantTime, MethodExpr.parse("getNext") -> ConstantTime))

        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(impls, Map("LinkedList" -> linkedList, "GenericHeap" -> genericHeap), decls, listAdt)

        assert(res.items == Set(linkedListResult))
      }
    }

    describe("priority queue ADT") {
      val linkedListPQResult = DataStructureChoice(
        Set("LinkedList"),
        Map(MethodExpr.parse("getSmallest") -> AffineBigOCombo(LinearTime, Map()),
          MethodExpr.parse("insertAnywhere!") -> AffineBigOCombo(ConstantTime, Map())))

      val heapResult = DataStructureChoice(
        Set("GenericHeap"),
        Map(MethodExpr.parse("getSmallest") -> AffineBigOCombo(ConstantTime, Map()),
          MethodExpr.parse("insertAnywhere!") -> Impl.rhs("g + log(n)").mapKeys(_.getAsNakedName)))

      it("can choose the Pareto-optimal options for a PriorityQueue adt") {
        val pQueueAdt = AbstractDataType(Map(),
          Map(MethodExpr.parse("getSmallest") -> ConstantTime,
            MethodExpr.parse("insertAnywhere!") -> ConstantTime)
        )

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(
          impls, Map("LinkedList" -> linkedList, "GenericHeap" -> genericHeap), decls, pQueueAdt)

        assert(res.items == Set(linkedListPQResult, heapResult))
      }

      it("can choose the best options for a PriorityQueue adt") {
        val pQueueAdt = AbstractDataType(Map(),
          Map(MethodExpr.parse("getSmallest") -> ConstantTime,
            MethodExpr.parse("insertAnywhere!") -> ConstantTime)
        )

        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(
          impls, Map("LinkedList" -> linkedList, "GenericHeap" -> genericHeap), decls, pQueueAdt)

        assert(res.items == Set(heapResult))
      }
    }
  }
}
