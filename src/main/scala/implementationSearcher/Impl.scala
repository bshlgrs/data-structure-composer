package implementationSearcher

import implementationSearcher.ImplLhs._
import parsers.MainParser
import shared._


/**
  * Created by buck on 7/25/16.
  *
  * Something like
  *

countBetweenBy[f] <- unorderedEach[f]
getMaximum <- getFirstBy[valueOrdering]
getMinimum <- getLastBy[valueOrdering]
deleteMinimumBy![f] <- getMinimumBy[f] + deleteNode!

  */
case class Impl(lhs: ImplLhs, rhs: AffineBigOCombo[MethodExpr]) {
  override def toString: String = {
    s"$lhs <- $rhs"
  }

  def addConditions(conditions: ImplPredicateMap): Impl = {
    Impl(lhs.addConditions(conditions), rhs)
  }

  // Suppose you have an implementation, like

  // f[x] <- g[x]

  // and you have implementations

  // g[y] <- y * n
  // g[y] if y.foo <- y * log(n)

  // This method returns

  // f[x] <- x * n
  // f[x] if x.foo <- x * log(n)
  def bindToAllOptions(searchResult: UnfreeImplSet): DominanceFrontier[UnnamedImpl] =
    unboundCostTuples(searchResult) match {
      case Nil => DominanceFrontier.fromSet(Set(this.copy(rhs = this.boundCost(searchResult))))
      case (methodExpr, methodCostWeight) :: other => {
        val otherwiseSubbedImpls = this.copy(rhs = rhs.filterKeys(_ != methodExpr)).bindToAllOptions(searchResult)

        val optionsAndConditions =
          searchResult.implsWhichMatchMethodExpr(methodExpr,
            ParameterList(lhs.conditions, searchResult.declarations(lhs.name).parameters))

        DominanceFrontier.fromSet(for {
          unfreeImpl <- otherwiseSubbedImpls.items
          optionImpl <- optionsAndConditions
        } yield {
          UnnamedImpl(
            lhs.conditions.and(optionImpl.predicates),
            unfreeImpl.cost + optionImpl.cost * methodCostWeight)
        })
      }
    }


  // Suppose you have the impl

  // f[x] if x.foo <- x

  // and you want to bind it to f[y], in a context where the parameter list has y in it

  // you get back `if y.foo <- y`
  def bindToContext(methodExpr: MethodExpr, unfreeImplSet: UnfreeImplSet, list: ParameterList): Set[UnnamedImpl] = {

    val parameters = unfreeImplSet.declarations(lhs.name).parameters

    assert(methodExpr.args.length == parameters.length,
      s"Assertion failed: bindToContext called on $this with methodExpr $methodExpr.\n" +
        s"These have a different number of args: $parameters vs ${methodExpr.args}.")


    val conditionsAndRhses: List[Set[UnnamedImpl]] = methodExpr.args.zipWithIndex.map({case (f: FunctionExpr, idx) =>
      val that = this
      val relevantParamName = parameters(idx)
      rhs.weights.get(MethodExpr(relevantParamName, Nil)) match {
        case None =>
          Set(UnnamedImpl(ImplPredicateMap.empty, AffineBigOCombo[MethodExpr](ConstantTime, Map())))
        case Some(weightOfParam) =>
          f.getConditionsAndCosts(lhs.conditions.get(parameters(idx)), unfreeImplSet, list)
            .items.map((x: UnnamedImpl) => x.copy(cost = x.cost * weightOfParam))
      }
    })

    println(s"conditionsAndRhses is $conditionsAndRhses")

    val combinationsOfImpls: Set[List[UnnamedImpl]] = Utils.cartesianProducts(conditionsAndRhses)

    for {
      conditionsAndRhses <- combinationsOfImpls
    } yield {
      val (conditionsList, rhsList) = conditionsAndRhses.map((x) => x.predicates -> x.cost).unzip
      val finalConditionsMap = conditionsList.reduceOption(_.and(_)).getOrElse(ImplPredicateMap.empty)
      val finalRhs = rhsList.reduceOption(_ + _).getOrElse(rhs) + rhs.bias
      UnnamedImpl(finalConditionsMap, finalRhs)
    }
  }

  def unboundCostTuples(unfreeImplSet: UnfreeImplSet): List[(MethodExpr, BigOLiteral)] = {
    rhs.filterKeys(!this.isMethodExprBound(unfreeImplSet, _)).weights.toList
  }

  def boundCost(unfreeImplSet: UnfreeImplSet): AffineBigOCombo[MethodExpr] = {
    rhs.filterKeys(this.isMethodExprBound(unfreeImplSet, _))
  }

  private def isMethodExprBound(unfreeImplSet: UnfreeImplSet, methodExpr: MethodExpr): Boolean = {
    (unfreeImplSet.boundVariables ++ unfreeImplSet.declarations(this.lhs.name).parameters).contains(methodExpr.name)
  }

  def getNames: Set[MethodName] = rhs.weights.keys.map(_.name).toSet

  def unnamed = UnnamedImpl(lhs.conditions, rhs)
}

object Impl {
  def apply(string: String): Impl = {
    MainParser.impl.parse(string).get.value._1
  }

  type Rhs = AffineBigOCombo[MethodExpr]

  def rhs(string: String): Rhs = {
    MainParser.implRhs.parse(string).get.value
  }

  implicit object ImplPartialOrdering extends PartialOrdering[Impl] {
    def partialCompare(x: Impl, y: Impl): DominanceRelationship = {
      if (x.lhs.name != y.lhs.name) {
        NeitherDominates
      } else {
        UnnamedImpl.UnnamedImplPartialOrdering.partialCompare(x.unnamed, y.unnamed)
      }
    }
  }
}
