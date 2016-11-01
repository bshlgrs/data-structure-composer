package tests

/**
  * Created by buck on 9/12/16.
  */

import cli.DataStructureChooserCli
import implementationSearcher._
import org.scalatest.FunSpec
import parsers.MainParser
import shared._

class StandardLibraryChoosingSpec extends FunSpec {
  val minStackAdt = MainParser.nakedAdt.parse("""
    adt MinStack {
      insertLast! -> 1
      deleteLast! -> 1
      getByIndex -> 1
      getMinimum -> 1
    }""".trim()).get.value

  describe("simple choosing with stdlib impls") {
    val impls = DataStructureChooserCli.impls
    val decls = DataStructureChooserCli.decls
    val structures = DataStructureChooserCli.dataStructures
    val library = ImplLibrary(impls, decls, structures)

    describe("with min stack") {
      it("can correctly evaluate the performance of a generic heap and vector list") {
        val res = Chooser.getRelevantTimesForDataStructures(
          library,
          Set(structures("Heap"), structures("VectorList"))
        )

        assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- 1")))
        assert(res.getNamed("insertLast!") == Set(Impl("insertLast! <- log(n)")))
      }

      it("can correctly evaluate the performance of a stack min memoizer and vector list") {
        val res = Chooser.getRelevantTimesForDataStructures(
          library,
          Set(structures("StackMinMemoizer"), structures("VectorList"))
        )

        assert(res.getNamed("getMinimum") == Set(Impl("getMinimum <- 1")))
        assert(res.getNamed("insertLast!") == Set(Impl("insertLast! <- 1")))
      }
    }
  }

  describe("data structure analysis") {
    it("can do a stack") {
      val adt = MainParser.nakedAdt.parse("""
        adt Stack {
          insertLast! -> 1
          deleteLast! -> 1
          getByIndex -> 1
          updateNode! -> 1
        }""".trim()).get.value

      val res = DataStructureChooserCli.chooseDataStructures(adt)

      assert(res.items.head.choices == Set("VectorList"))
    }

    it("can do a min-stack") {
      val res = DataStructureChooserCli.chooseDataStructures(minStackAdt)

      DataStructureChooserCli.printResults(res)

      assert(res.items.head.choices == Set("StackMinMemoizer","VectorList"))
    }

    it("can do a stack with contains") {
      val res = DataStructureChooserCli.chooseDataStructures(MainParser.nakedAdt.parse("""
        adt Stack {
          insertLast! -> 1
          deleteLast! -> 1
          getByIndex -> 1
          updateNode! -> 1
          contains -> 1
        }""".trim()).get.value)

      DataStructureChooserCli.printResults(res)

      assert(res.items.head.choices == Set("HistogramHashMap","VectorList"))
    }

    it("can do a set which you never delete from") {
      val res = DataStructureChooserCli.chooseDataStructures(MainParser.nakedAdt.parse("""
        adt NeverDeletedSet {
          insertLast! -> 1
          contains -> 1
        }""".trim()).get.value)

      DataStructureChooserCli.printResults(res)

      assert(res.items.head.choices == Set("HistogramHashMap"))
    }

    it("knows how to use parameterized data structures") {
      val adt = MainParser.nakedAdt.parse("""
        adt RmqList {
          insertAtIndex! -> 1
          getByIndex -> 1
          rangeMinimumQuery -> 1
        }""".trim()).get.value

      val res = DataStructureChooserCli.chooseDataStructures(adt)

      DataStructureChooserCli.printResults(res)

      assert(res.items.head.choices == Set("AugmentedRedBlackOrderStatisticTreeList"))
    }
  }
}
