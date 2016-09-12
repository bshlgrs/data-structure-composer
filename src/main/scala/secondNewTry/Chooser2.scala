package secondNewTry

import shared._

import scala.collection.mutable

/**
  * Created by buck on 7/25/16.
  */
object Chooser2 {
  type CostFunction = AffineFunction[String, BigOLiteral]

  val implLibrary = Set(
    Impl(ImplLhs("getFirst"), ImplRhs(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("getNext"), ImplRhs(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("getByIndex"), ImplRhs(ConstantTime,
      Map(MethodExpr("getFirst") -> ConstantTime, MethodExpr("getNext") -> LinearTime))),
    Impl(ImplLhs("getLast"), ImplRhs(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),
    Impl(ImplLhs("getPrev"), ImplRhs(ConstantTime, Map(MethodExpr("getByIndex") -> ConstantTime))),

    Impl(ImplLhs("unorderedEach", List("f")),
      ImplRhs(ConstantTime, Map(MethodExpr("each", List(NamedFunctionExpr("f"))) -> ConstantTime))),
    Impl(ImplLhs("each", List("f")),
      ImplRhs(ConstantTime, Map(MethodExpr("getByIndex") -> LinearTime, MethodExpr("f") -> LinearTime))),
    Impl(ImplLhs("each", List("f")),
      ImplRhs(ConstantTime, Map(MethodExpr("getFirst") -> LinearTime, MethodExpr("getNext") -> LinearTime, MethodExpr("f") -> LinearTime)))
  )

  def getAllTimes(impls: Set[Impl]): SearchResult = {
    val queue = mutable.PriorityQueue[(BigOLiteral, UnfreeImpl)]()(Ordering.by((x: (BigOLiteral, UnfreeImpl)) => x._1).reverse)

    queue ++= impls.flatMap(_.toUnfreeImpl).map((u: UnfreeImpl) => (u.cost, u)).toList

    var searchResult = SearchResult()

    def queuePlusSelected: Iterator[Impl] = queue.toIterator.map(_._2) ++ searchResult.allImpls

    while (queue.nonEmpty) {
      val (time, unfreeImpl) = queue.dequeue()

      if (searchResult.isOtherImplUseful(unfreeImpl)) {
        searchResult = searchResult.addImpl(unfreeImpl)

        for (impl <- impls) {
          // if we don't already have anything selected
          val neighborUnfreeImpls = impl.bindToAllOptions(searchResult)

          neighborUnfreeImpls.foreach((u: UnfreeImpl) =>
            if (searchResult.isOtherImplUseful(u)) {
              queue ++= List((u.cost, u))
            }
          )
        }
      }
    }

    searchResult
  }

  def getAllTimesForDataStructures(impls: Set[Impl], dataStructures: Set[SimpleDataStructure]) = {
    getAllTimes(impls.union(dataStructures.flatMap(_.sourcedImpls)))
  }

  def main(args: Array[String]) {
    println(getAllTimesForDataStructures(implLibrary, DataStructureLibrary.library))
  }
}
