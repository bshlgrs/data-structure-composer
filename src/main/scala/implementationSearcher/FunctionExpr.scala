package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import implementationSearcher.UnfreeImpl.Rhs
import shared.{ZeroTime, BigOLiteral, ConstantTime, Utils}

/**
  * Created by buck on 7/31/16.
  */

abstract class FunctionExpr {
  def properties(conditions: Map[String, Set[FunctionProperty]]): Set[FunctionProperty] = this match {
    case NamedFunctionExpr(name) => conditions(name)
    case AnonymousFunctionExpr(defaultProperties, _) => defaultProperties
  }

  def getNames: Set[String] = this match {
    case NamedFunctionExpr(name) => Set(name)
    case AnonymousFunctionExpr(props, args) => args.weights.keys.map(_.name).toSet
  }

  def getConditionsAndCosts(conditions: Set[String], scope: Scope): Set[(ImplPredicateMap, Rhs)] = this match {
    case AnonymousFunctionExpr(properties, fRhs) => {
      // If the anonymous function has the necessary properties, then add no conditions and continue
      if (conditions.subsetOf(properties)) {
        val costsOfParamsInMethodExprNames: List[Set[Rhs]] = fRhs.weights.map({case (name, weight) => {
          // Suppose that this FunctionExpr is _ <- n * f.

          // Maybe f is a globally defined function. So look for it in the searchResult:
          val alreadyChosenImpls = scope.globals.get(name)
          if (alreadyChosenImpls.nonEmpty) {
            assert(alreadyChosenImpls.forall(_.lhs.parameters.isEmpty))
            alreadyChosenImpls.filter(_.lhs.conditions.isEmpty).map(_.rhs)
          }
          // Otherwise, maybe it's a locally bound variable. So check whether it's in implLhs.parameters.
          else if (scope.locals.contains(name.name)) {
            Set(AffineBigOCombo(ZeroTime, Map(name -> weight)))
          }
          // Otherwise, just assume that it's a globally defined function which has not been defined.
          // So this anonymous method cannot be executed.
          else {
            Set[Rhs]()
          }
        }}).toList


        val costOptions: Set[List[Rhs]] = Utils.cartesianProducts(costsOfParamsInMethodExprNames)

        costOptions.map((x) => {
          val overallCost = x.reduceOption(_ + _).getOrElse(AffineBigOCombo[MethodName](ConstantTime, Map()))
          ImplPredicateMap.empty -> overallCost
        })
      } else
        Set()
    }
    case NamedFunctionExpr(name) => {
      val that = this

      // This name might be locally bound or globally bound.
      // If it's locally bound:
      if (scope.locals.contains(name)) {
        Set(
          (
            ImplPredicateMap(Map(name -> conditions)),
            AffineBigOCombo[MethodName](ZeroTime, Map(MethodName(name) -> ConstantTime))
          )
        )
      } else {
        val that = this
        // Otherwise it's globally bound, so look for an implementation which has already been sorted.

        // Currently I am not allowing higher-order methods here. So there can only be one implementation.
        scope.globals.get(MethodName(name)) match {
          case x: Set[UnfreeImpl] if x.size == 1 =>
            val oneImplementation: UnfreeImpl = x.head

            if (oneImplementation.lhs.parameters.isEmpty) {
              Set((ImplPredicateMap.empty, oneImplementation.rhs))
            } else {
              ???
            }
          case x: Set[UnfreeImpl] if x.isEmpty =>
            Set()
          case x: Set[UnfreeImpl] if x.size > 1 =>
            // One day I will extend this code to allow higher-order methods; at that point I'll fill in this part of the code.
            ???
        }
      }
    }
  }
}

object UnderscoreFunctionExpr extends AnonymousFunctionExpr(Set(), AffineBigOCombo(ConstantTime, Map()))

case class NamedFunctionExpr(name: String) extends FunctionExpr {
  override def toString = name
}

case class AnonymousFunctionExpr(properties: Set[String], cost: AffineBigOCombo[MethodName] = AffineBigOCombo(ConstantTime, Map())) extends FunctionExpr {
  override def toString = s"_{${properties.mkString(",")}} <- $cost"
}
