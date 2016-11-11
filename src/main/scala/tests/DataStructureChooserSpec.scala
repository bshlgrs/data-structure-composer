package tests

/**
  * Created by buck on 9/12/16.
  */

import cli.DataStructureChooserCli
import implementationSearcher.{Impl, _}
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
      |    deleteNode! <- 1
      |    insertFirst! <- 1
      |}""".stripMargin, decls)

  val genericHeap = DataStructure(
    """ds GenericHeap[g] {
      |    updateNode! <- log(n)
      |    getFirstBy[f] <- 1
      |    insertAtIndex! <- log(n)
      |    unorderedEach <- n
      |    deleteNode! <- log(n)
      |}""".stripMargin, decls)


  val heap = DataStructure(
    """ds Heap {
      |    getMinimum <- 1
      |    updateNode! <- log(n)
      |    insertAtIndex! <- log(n)
      |    unorderedEach <- n
      |    deleteNode! <- log(n)
      |}""".stripMargin, decls)


  val invertibleReductionMemoizer = DataStructure(
    """ds InvertibleReductionMemoizer[f] if f.invertible, f.commutative {
      |    reduce[f] <- 1
      |    insertAtIndex! <- 1
      |    updateNode! <- 1 + getByIndex
      |}""".stripMargin, decls)

  val library: ImplLibrary = ImplLibrary(impls, decls,
    Map("GenericHeap" -> genericHeap, "LinkedList" -> linkedList, "InvertibleReductionMemoizer" -> invertibleReductionMemoizer))


  describe("data structure analysis") {
    describe("integration tests") {
      it("can do a linked list") {
        val res = Chooser.getAllTimesForDataStructure(library, linkedList)

        assert(res.getNamedWithoutSource("getByIndex") == Set(Impl("getByIndex <- n")))
        assert(res.getNamedWithoutSource("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamedWithoutSource("getMinimum") == Set(Impl("getMinimum <- n")))
        assert(res.getNamedWithoutSource("getNext") == Set(Impl("getNext <- 1")))
        assert(res.getNamedWithoutSource("updateNode!") == Set(Impl("updateNode! <- 1")))
      }

      it("can do a heap") {
        val res = Chooser.getAllTimesForDataStructure(library, heap)

        assert(res.getNamedWithoutSource("getByIndex") == Set(Impl("getByIndex <- n")))
        assert(res.getNamedWithoutSource("getMinimum") == Set(Impl("getMinimum <- 1")))
        assert(res.getNamedWithoutSource("getFirst") == Set(Impl("getFirst <- n")))
        assert(res.getNamedWithoutSource("updateNode!") == Set(Impl("updateNode! <- log(n)")))
      }

      it("can do linked-list + heap") {
        val res =
          Chooser.getAllTimesForDataStructures(library, Set(linkedList, heap))

        assert(res.readMethods.getNamedWithoutSource("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.writeMethods.getNamedWithoutSource("insertAnywhere!") == Set(Impl("insertAnywhere! <- log(n)")))
        assert(res.readMethods.getNamedWithoutSource("getMinimum") == Set(Impl("getMinimum <- 1")))
      }

      describe("doing a generic heap") {
        val res = Chooser.getAllTimesForDataStructure(library, genericHeap)

        it("succeeds at the read methods") {
          assert(res.getNamedWithoutSource("getByIndex") == Set(Impl("getByIndex <- n")))
          assert(res.getNamedWithoutSource("getMinimum") == Set(Impl("getMinimum <- 1")))
          assert(res.getNamedWithoutSource("getFirst") == Set(Impl("getFirst <- n")))
          assert(res.getNamedWithoutSource("getByIndex") == Set(Impl("getByIndex <- n")))
          assert(res.getNamedWithoutSource("getNext") == Set(Impl("getNext <- n")))
        }

        it("succeeds at the write methods") {
          assert(res.getNamedWithoutSource("updateNode!") == Set(Impl("updateNode! <- log(n)")))
        }
      }

      it("can do linked-list + generic heap") {
        val res = Chooser.getAllTimesForDataStructures(library, Set(linkedList, genericHeap)).fullUnfreeImplSet

        assert(res.getNamedWithoutSource("getFirst") == Set(Impl("getFirst <- 1")))
        assert(res.getNamedWithoutSource("insertAnywhere!") == Set(Impl("insertAnywhere! <- log(n)")))
        assert(res.getNamedWithoutSource("getMinimum") == Set(Impl("getMinimum <- 1")))
      }

      describe("with invertible reduction memoizer") {
        it("does getRelevantTimes correctly") {
          val res = Chooser.getAllTimesForDataStructures(library, Set(invertibleReductionMemoizer, linkedList)).fullUnfreeImplSet

          assert(res.getNamedWithoutSource("getSum") == Set(Impl("getSum <- 1")))

          // This is because the equals method distinguishes between the maps Map() and Map(f -> Set()).
          val otherImpl = {
            val thing = Impl("reduce[f] <- n + n * f")
            thing.copy(lhs = thing.lhs.copy(conditions = ImplPredicateMap(Map())))
          }

          assert(res.getNamedWithoutSource("reduce") == Set(
            Impl("reduce[f] if f.invertible, f.commutative <- 1"),
            otherImpl))
        }
      }
    }
  }

  describe("adt analysis") {
    describe("List adt") {
      val listAdt = AbstractDataType(Map(), Map(MethodExpr.parse("getFirst") -> ConstantTime, MethodExpr.parse("getNext") -> ConstantTime))

      val linkedListResult =
        Map(MethodExpr.parse("getFirst") -> ConstantTime,
          MethodExpr.parse("getNext") -> ConstantTime)

      it("can choose the Pareto-optimal options for a List adt") {
        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(library, listAdt)

        assert(res.items.map(_.resultTimes) == Set(linkedListResult))
      }

      it("can choose the best options for a List adt") {
        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, listAdt)

        assert(res.items.map(_.resultTimes) == Set(linkedListResult))
      }
    }

    describe("priority queue ADT") {
      val linkedListPQResult = Map(MethodExpr.parse("getMinimum") -> LinearTime,
          MethodExpr.parse("insertAnywhere!") -> ConstantTime,
          MethodExpr.parse("deleteNode!") -> ConstantTime)


      val heapResult = Map(
        MethodExpr.parse("getMinimum") -> ConstantTime,
        MethodExpr.parse("insertAnywhere!") -> LogTime,
        MethodExpr.parse("deleteNode!") -> LogTime)

      val pQueueAdt = AbstractDataType(Map(),
        Map(MethodExpr.parse("getMinimum") -> ConstantTime,
          MethodExpr.parse("insertAnywhere!") -> ConstantTime,
          MethodExpr.parse("deleteNode!") -> ConstantTime)
      )

      it("can choose the Pareto-optimal options for a PriorityQueue adt") {
        val res2 = Chooser.getAllTimesForDataStructures(library, Set(linkedList))

        val res = Chooser.allParetoOptimalDataStructureCombosForAdt(library, pQueueAdt)

        assert(res.items.map(_.resultTimes) == Set(linkedListPQResult, heapResult))
      }

      it("can choose the best options for a PriorityQueue adt") {
        val res = Chooser.allMinTotalCostParetoOptimalDataStructureCombosForAdt(library, pQueueAdt)

        assert(res.items.map(_.resultTimes) == Set(heapResult))
      }
    }
  }
}
