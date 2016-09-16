package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty

/**
  * Created by buck on 7/31/16.
  */
// something like `unorderedEach[f]` or `insertAtEnd!` or `maxBy[func{commutative} <- f * n]`
case class MethodExpr(name: MethodName, args: List[FunctionExpr]) {
  override def toString: String = {
    val argsString = if (args.isEmpty) "" else s"[${args.map(_.toString).mkString(", ")}]"
    return name.name + argsString
  }

//  // this should probably return a dominance frontier :'(
//  def getFastestRelevantImplementationIfAny(impls: Set[UnfreeImpl]): Option[UnfreeImpl] = {
//    /// THIS IS FUCKED UP
//    // I should actually return a dominance frontier.
//    impls.filter((x) => canBeImplementedBy(x.lhs)).toList.sortBy(_.minCost).headOption
//  }
}

object MethodExpr {
  def apply(name: String, args: List[FunctionExpr] = Nil): MethodExpr = MethodExpr(MethodName(name), args)

}
