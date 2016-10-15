package implementationSearcher


import parsers.MainParser
import shared._

import scala.collection.mutable
import scala.io.Source

/**
  * Created by buck on 7/25/16.
  */
object Chooser {
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

  def getRelevantTimesForDataStructures(impls: Set[Impl],
                                        structures: Set[DataStructure]): SearchResult = {
    val allProvidedReadImplementations: Set[UnfreeImpl] = structures.flatMap(_.readMethods)

    val bestReadImplementations: SearchResult = getAllTimes(allProvidedReadImplementations.map(_.toImpl) ++ impls)

    val allWriteImplementations: Set[SearchResult] = structures.map((s) =>
      getAllTimes((s.writeMethods ++ bestReadImplementations.allImpls).map(_.toImpl) ++ impls))

    val combinedWriteImplementations: SearchResult = allWriteImplementations.reduceOption(_.product(_)).getOrElse(SearchResult())

    bestReadImplementations.addImpls(combinedWriteImplementations.allImpls)
  }

  def allParetoOptimalDataStructureCombosForAdt(impls: Set[Impl],
                                                structures: Set[DataStructure],
                                                adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    val results = structures.subsets().map((x) => x -> getRelevantTimesForDataStructures(impls, x)).toSet

    val choicesSet: Set[DataStructureChoice] = results.flatMap({ case (set: Set[DataStructure], sr: SearchResult) => {
      val methods = adt.methods.keys.map((methodExpr: MethodExpr) => {
        // TODO: let this be a proper dominance frontier
        methodExpr -> sr.implsWhichMatchMethodExpr(methodExpr, Scope(Map(), sr)).headOption.map(_._3)
      }).toMap

      if (methods.forall(_._2.isDefined))
        Set[DataStructureChoice](DataStructureChoice(set.map(_.name), methods.mapValues(_.get)))
      else
        Set[DataStructureChoice]()
    }})

    // A dominance frontier on choices, ranked by simplicity and also on the methods which the ADT cares about.
    DominanceFrontier.fromSet(choicesSet)
  }

  // this is the ultimate method
  def allMinTotalCostParetoOptimalDataStructureCombosForAdt(impls: Set[Impl],
                                                            structures: Set[DataStructure],
                                                            adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    val frontier = allParetoOptimalDataStructureCombosForAdt(impls, structures, adt)

    val bestTime = frontier.items.map(_.overallTimeForAdt(adt)).min

    frontier.filter(_.overallTimeForAdt(adt) == bestTime)
  }
}

