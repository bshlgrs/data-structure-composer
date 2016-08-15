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

  def getAllTimes(impls: Set[Impl]): Map[ImplLhs, CostFunction] = {
    val queue = mutable.PriorityQueue[(BigOLiteral, UnfreeImpl)]()(Ordering.by((x: (BigOLiteral, UnfreeImpl)) => x._1).reverse)

    val selectedImpls = mutable.Set[Impl]()

    def queuePlusSelected: Iterator[Impl] = queue.toIterator.map(_._2) ++ selectedImpls

    while (queue.nonEmpty) {
      val (time, impl) = queue.dequeue()

      if (!(queuePlusSelected.contains(impl) || selectedImpls.exists(_.lhs.dominance(impl.lhs)))) {
        selectedImpls.add(impl)

        for (neighbor <- impls) {
          // if we don't already have anything selected
          if (!selectedImpls.exists(_.lhs.dominance(neighbor.lhs))) {
            neighbor.runtime(selectedImpls)
          }
        }
      }
    }
    // val queue = priority queue from Impl to CostFunction
    // while queue:
    //   impl = queue.pop
    //   if node is already totally explored, continue. (Remember to take predicates into account)
    //   set official time for impl to this time
    //   for impl in implementations which use this implrhs: (might as well just filter)
    //     if it's now doable but not already in the queue, stick it in! (For efficiency, don't do this if it's dominated
    //        by something already in the queue)
    // return costs
    ???
  }

  def getAllTimesForDataStructures(impls: Set[Impl], dataStructures: Set[SimpleDataStructure]) = {

  }

  def main(args: Array[String]) {
    getAllTimesForDataStructures(implLibrary, DataStructureLibrary.library)
  }
}
