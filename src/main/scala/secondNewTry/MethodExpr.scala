package secondNewTry

/**
  * Created by buck on 7/31/16.
  */
// something like `unorderedEach[f]` or `insertAtEnd!` or `maxBy[func{commutative} <- f * n]`
case class MethodExpr(name: String, args: List[FunctionExpr] = Nil) {
  def canBeImplementedByAnyOf(impls: Set[UnfreeImpl]): Boolean = {
    impls.map(_.lhs).exists(this.canBeImplementedBy)
  }

  def canBeImplementedBy(implLhs: ImplLhs): Boolean = {
    if (name == implLhs.name) {
      implLhs.normalizedConditionsList.zip(args).forall({
        case (conditions: Set[String], functionExpr: FunctionExpr) =>
          conditions.subsetOf(functionExpr.properties)
      })
    } else {
      false
    }
  }

  def bind(impls: Set[UnfreeImpl]): DominanceFrontier[UnfreeImpl] = {
    impls.filter((x) => canBeImplementedBy(x.lhs))

    ???
  }
}
