package implementationSearcher

import parsers.MainParser
import shared._

import scala.collection.mutable

/**
  * Created by buck on 7/25/16.
  */
object Chooser {
  val implLibrary = Set(
    Impl(ImplLhs("getFirst"), AffineBigOCombo(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("getNext"), AffineBigOCombo(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("getByIndex"), AffineBigOCombo(ConstantTime,
      Map(MethodExpr("getFirst") -> ConstantTime, MethodExpr("getNext") -> LinearTime))),
    Impl(ImplLhs("getLast"), AffineBigOCombo(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("getPrev"), AffineBigOCombo(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("unorderedEach", List("f")),
      AffineBigOCombo(ConstantTime, Map(MethodExpr("each", List(NamedFunctionExpr("f"))) -> ConstantTime))),
    Impl(ImplLhs("each", List("f")),
      AffineBigOCombo(ConstantTime, Map(MethodExpr("getByIndex") -> LinearTime, MethodExpr("f") -> LinearTime))),
    Impl(ImplLhs("each", List("f")),
      AffineBigOCombo(ConstantTime, Map(MethodExpr("getFirst") -> LinearTime, MethodExpr("getNext") -> LinearTime, MethodExpr("f") -> LinearTime))),
    Impl(ImplLhs("getMax"), AffineBigOCombo(ConstantTime, Map(MethodExpr("reduce", List(AnonymousFunctionExpr(Set("commutative")))) -> ConstantTime))),
    Impl(ImplLhs("getMaxEarlyBiased"), AffineBigOCombo(ConstantTime, Map(MethodExpr("reduce", List(UnderscoreFunctionExpr)) -> ConstantTime))),
    Impl(ImplLhs("reduce", List("f")),
      AffineBigOCombo(ConstantTime, Map(MethodExpr("each", List(NamedFunctionExpr("f"))) -> ConstantTime))),
    Impl(ImplLhs("reduce", List("f"), Some(ImplPredicateList(List(Set("commutative"))))),
      AffineBigOCombo(ConstantTime, Map(MethodExpr("unorderedEach", List(NamedFunctionExpr("f"))) -> ConstantTime))),
    Impl(ImplLhs("getSum"),
      AffineBigOCombo(ConstantTime, Map(MethodExpr("reduce", List(AnonymousFunctionExpr(Set("commutative", "invertible")))) -> ConstantTime)))
  )

  val autoImplLibrary = MainParser.impls.parse(
    """getFirst <- getByIndex
      |getNext <- getByIndex
      |getByIndex <- getFirst + n * getNext
      |getLast <- getByIndex
      |getPrev <- getByIndex
      |each[f] <- n * getByIndex + n * f
    """.stripMargin).get.value.toSet

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

//      println(s"----------\n\nQueue = $queue\ntime = $time\nunfreeImpl = $unfreeImpl. Search result:\n ${searchResult.toLongString}")

      if (searchResult.isOtherImplUseful(unfreeImpl)) {
//        println("It's useful! Adding it now...")
        searchResult = searchResult.addImpl(unfreeImpl)

        for (otherImpl <- impls) {
          // So we have a random impl. Let's see if the unfreeImpl we just settled on is useful for that impl.
          // It's only useful if unfreeImpl's methodName is used by the rhs of the otherImpl (this condition is weaker than it could be)

          val otherImplMethodsUsed = otherImpl.rhs.m.keys.collect({ case MethodExpr(name, _) => name }).toList

          if (otherImplMethodsUsed.contains(unfreeImpl.lhs.name)) {
//            println(s"Wow, this is used by $otherImpl")
            val neighborUnfreeImpls = otherImpl.bindToAllOptions(searchResult)

//            println(s"neighborUnfreeImpls are $neighborUnfreeImpls")
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

  def getAllTimesForDataStructures(impls: Set[Impl], dataStructures: Set[SimpleDataStructure]) = {
    getAllTimes(impls.union(dataStructures.flatMap(_.sourcedImpls)))
  }

  def getAllTimesForDataStructure(impls: Set[Impl], dataStructure: SimpleDataStructure) = {
    getAllTimes(impls.union(dataStructure.sourcedImpls))
  }

  def main(args: Array[String]) {
    println(getAllTimesForDataStructure(autoImplLibrary, DataStructureLibrary.library("ArrayList")).toLongString)
  }
}

