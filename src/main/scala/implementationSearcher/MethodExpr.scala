package implementationSearcher

/**
  * Created by buck on 7/31/16.
  */
// something like `unorderedEach[f]` or `insertAtEnd!` or `maxBy[func{commutative} <- f * n]`
case class MethodExpr(name: MethodName, args: List[FunctionExpr]) {
  def canBeImplementedByAnyOf(impls: Set[UnfreeImpl]): Boolean = {
    impls.map(_.lhs).exists(this.canBeImplementedBy)
  }

  def canBeImplementedBy(implLhs: ImplLhs): Boolean = {
    if (name == implLhs.name) {
      implLhs.conditions.list.zip(args).forall({
        case (conditions: Set[String], functionExpr: FunctionExpr) =>
          conditions.subsetOf(functionExpr.properties)
      })
    } else {
      false
    }
  }

  def conditions: ImplPredicateList = ImplPredicateList(args.map(_.conditions))

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
