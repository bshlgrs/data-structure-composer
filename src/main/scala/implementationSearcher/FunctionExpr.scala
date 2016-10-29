package implementationSearcher

import implementationSearcher.Impl.Rhs
import implementationSearcher.ImplLhs.FunctionProperty
import implementationSearcher.Impl.Rhs
import shared._

import scala.collection.Searching.SearchResult

/**
  * Created by buck on 7/31/16.
  *
  * FunctionExprs are things that can be parameters to MethodExprs.
  *
  * _ <- n
  * _{commutative} <- f * n
  * f
  *
  * are all examples
  */

abstract class FunctionExpr {
  def properties(conditions: Map[String, Set[FunctionProperty]]): Set[FunctionProperty] = this match {
    case NamedFunctionExpr(name) => conditions(name.name)
    case AnonymousFunctionExpr(defaultProperties, _) => defaultProperties
  }

  def getNames: Set[MethodName] = this match {
    case NamedFunctionExpr(name) => Set(name)
    case AnonymousFunctionExpr(props, args) => args.weights.keys.toSet
  }

  def alphaConvert(map: Map[MethodName, MethodName]): FunctionExpr = this match {
    case NamedFunctionExpr(name) => NamedFunctionExpr(map.getOrElse(name, name))
    case AnonymousFunctionExpr(props, args) => AnonymousFunctionExpr(props, args.mapKeys((x) => map.getOrElse(x, x)))
  }

  // All the different combinations of costs and required method conditions that you could get by implementing this FunctionExpr
  def getConditionsAndCosts(conditions: Set[String], unfreeImplSet: UnfreeImplSet, locals: Set[MethodName]): DominanceFrontier[UnnamedImpl]
}

object UnderscoreFunctionExpr extends AnonymousFunctionExpr(Set(), AffineBigOCombo(ConstantTime, Map()))

case class NamedFunctionExpr(name: MethodName) extends FunctionExpr {
  override def toString = name.name

  def getConditionsAndCosts(conditions: Set[FunctionProperty], unfreeImplSet: UnfreeImplSet, locals: Set[MethodName]): DominanceFrontier[UnnamedImpl] = {
    val that = this

    // This name might be locally bound or globally bound.
    // If it's locally bound:
    if (locals.contains(name)) {
      DominanceFrontier.fromSet(Set(
        UnnamedImpl(
          ImplPredicateMap(Map(name -> conditions)),
          AffineBigOCombo[MethodExpr](ZeroTime, Map(MethodExpr(name, Nil) -> ConstantTime)))
      ))
    } else {
      val that = this
      // Otherwise it's globally bound, so look for an implementation which has already been sorted.

      // Currently I am not allowing higher-order methods here. So there can only be one implementation.
      unfreeImplSet.get(name) match {
        case x: Set[UnnamedImpl] if x.size == 1 =>
          val oneImplementation: UnnamedImpl = x.head

          if (oneImplementation.predicates.isEmpty) {
            DominanceFrontier.fromSet(Set(oneImplementation))
          } else {
            ???
          }
        case x: Set[UnnamedImpl] if x.isEmpty =>
          DominanceFrontier.empty[UnnamedImpl]
        case x: Set[UnnamedImpl] if x.size > 1 =>
          // One day I will extend this code to allow higher-order methods; at that point I'll fill in this part of the code.
          ???
      }
    }
  }
}

case class AnonymousFunctionExpr(properties: Set[String], cost: AffineBigOCombo[MethodName] = AffineBigOCombo(ConstantTime, Map())) extends FunctionExpr {
  override def toString = s"_{${properties.mkString(",")}} <- $cost"

  def getConditionsAndCosts(conditions: Set[String], unfreeImplSet: UnfreeImplSet, locals: Set[MethodName]): DominanceFrontier[UnnamedImpl] = {
    // If the anonymous function has the necessary properties, then add no conditions and continue
    if (conditions.subsetOf(properties)) {
      val costsOfParamsInMethodExprNames: List[Set[Rhs]] = cost.weights.map({case (name, weight) => {
        // Suppose that this FunctionExpr is _ <- n * f.

        // Maybe f is a globally defined function. So look for it in the searchResult:
        val alreadyChosenImpls = unfreeImplSet.get(name)
        if (alreadyChosenImpls.nonEmpty) {
          assert(alreadyChosenImpls.forall(_.predicates.isEmpty))
          alreadyChosenImpls.filter(_.predicates.isEmpty).map(_.cost)
        }
        // Otherwise, maybe it's a locally bound variable. So check whether it's in implLhs.parameters.
        else if (locals.contains(name)) {
          Set(AffineBigOCombo(ZeroTime, Map(MethodExpr(name, Nil) -> weight)))
        }
        // Otherwise, just assume that it's a globally defined function which has not been defined.
        // So this anonymous method cannot be executed.
        else {
          Set[Rhs]()
        }
      }}).toList


      val costOptions: Set[List[Rhs]] = Utils.cartesianProducts(costsOfParamsInMethodExprNames)

      DominanceFrontier.fromSet(costOptions.map((x) => {
        val overallCost = x.reduceOption(_ + _).getOrElse(AffineBigOCombo[MethodExpr](ConstantTime, Map()))
        UnnamedImpl(ImplPredicateMap.empty, overallCost)
      }))
    } else
    // otherwise you can't do it.
      DominanceFrontier.empty[UnnamedImpl]
  }
}
