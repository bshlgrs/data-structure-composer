package implementationSearcher


import parsers.MainParser
import shared._

import scala.collection.mutable
import scala.io.Source

/**
  * Created by buck on 7/25/16.
  */
object Chooser {
  lazy val libraryText = {
    Source.fromFile("data/implementations.txt")
      .getLines()
      .mkString("\n")
  }

  lazy val autoImplLibrary = MainParser.impls.parse(libraryText).get.value.toSet

  lazy val dataStructuresText = {
    Source.fromFile("data/data_structures.txt")
      .getLines()
      .mkString("\n")
  }

  lazy val dataStructuresLibrary: Map[String, DataStructure] = {
    MainParser.dataStructureFile.parse(dataStructuresText).get.value.map((x) => x.name -> x).toMap
  }

  // x[f] if x.foo <- 1
  // y[g] <- x[g]
  // should infer
  // y[g] if g.foo <- 1
//  val testLibrary = Set(
//    Impl(ImplLhs("x", List("f"), Some(ImplPredicateList(List(Set("foo"))))), AffineBigOCombo[MethodExpr](LogTime)),
//    Impl(ImplLhs("y", List("g")),
//      AffineBigOCombo[MethodExpr](ConstantTime, Map(MethodExpr("x", List(NamedFunctionExpr("g"))) -> ConstantTime)))
//  )

  def getAllTimes(impls: Set[Impl]): SearchResult = {
    val queue = mutable.PriorityQueue[(BigOLiteral, UnfreeImpl)]()(Ordering.by((x: (BigOLiteral, UnfreeImpl)) => x._1).reverse)

    queue ++= impls.flatMap(_.toUnfreeImpl).map((u: UnfreeImpl) => (u.cost, u)).toList

    var searchResult = SearchResult()

    def queuePlusSelected: Iterator[UnfreeImpl] = queue.toIterator.map(_._2) ++ searchResult.allImpls

    while (queue.nonEmpty) {
      val (time, unfreeImpl) = queue.dequeue()

      if (searchResult.isOtherImplUseful(unfreeImpl)) {
        searchResult = searchResult.addImpl(unfreeImpl)
//        println(searchResult.toLongString)
//        println(s"unfree impl is $unfreeImpl")

        for (otherImpl <- impls) {
          // So we have a random impl. Let's see if the unfreeImpl we just settled on is useful for that impl.
          // It's only useful if unfreeImpl's methodName is used by the rhs of the otherImpl (this condition is weaker than it could be)

          val otherImplMethodsUsed = otherImpl.getNames

          if (otherImplMethodsUsed.contains(unfreeImpl.lhs.name.name)) {
            val neighborUnfreeImpls = otherImpl.bindToAllOptions(searchResult)

            neighborUnfreeImpls.foreach((u: UnfreeImpl) =>
              if (searchResult.isOtherImplUseful(u)) {
                queue ++= List((u.cost, u))
              }
            )
          }
        }
      }
    }

    searchResult
  }

  def getAllTimesForDataStructure(impls: Set[Impl], dataStructure: DataStructure) = {
    getAllTimes(impls.union(dataStructure.sourcedImpls.map(_.toImpl)))
  }

  def main(args: Array[String]) {
    println(getAllTimesForDataStructure(autoImplLibrary, dataStructuresLibrary("VectorList")).toLongString)
  }

  def getRelevantTimesForDataStructures(impls: Set[Impl],
                                        structures: Set[DataStructure]): SearchResult = {
    val allProvidedReadImplementations: Set[UnfreeImpl] = structures.flatMap(_.readMethods)

    val bestReadImplementations: SearchResult = getAllTimes(allProvidedReadImplementations.map(_.toImpl) ++ impls)

    val allWriteImplementations: Set[SearchResult] = structures.map((s) =>
      getAllTimes((s.writeMethods ++ bestReadImplementations.allImpls).map(_.toImpl) ++ impls))

    val combinedWriteImplementations: SearchResult = allWriteImplementations.reduceOption(_.product(_)).getOrElse(SearchResult())

    bestReadImplementations.addImpls(combinedWriteImplementations.allImpls)
  }

  def bestDataStructureCombosForAdt(impls: Set[Impl],
                                    structures: Set[DataStructure],
                                    adt: AbstractDataType): Set[(Set[DataStructure], Map[MethodName, AffineBigOCombo[MethodName]])] = {

    // actually this needs to get the dominance frontier :/
    structures.subsets().map((x) => x -> getRelevantTimesForDataStructures(impls, x).bestFullyGeneralTimes).toSet
  }
}

