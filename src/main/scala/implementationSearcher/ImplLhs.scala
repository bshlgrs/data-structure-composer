package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import shared.{DominanceRelationship, PartialOrdering}

/**
  * Created by buck on 7/25/16.
  */
case class ImplLhs(name: MethodName, parameters: List[String], conditions: ImplPredicateList) {
  assert(parameters.length == conditions.list.length, s"Impl for ${name.name} failed :/")

  def addConditions(conditions: ImplPredicateList) = {
    this.copy(conditions = this.conditions.and(conditions))
  }

  def addConditions(conditions: ImplPredicateMap) = {
    this.copy(conditions = this.conditions.and(conditions.toList(parameters)))
  }

  override def toString: String = {
    val parametersString = parameters match {
      case Nil => ""
      case _ => s"[${parameters.mkString(", ")}]"
    }

    val conditionsString = if (conditions.isEmpty) "" else s"if ${conditions.toNiceString(parameters)}"

    s"${name.name}$parametersString $conditionsString"
  }

  def isMutating: Boolean = name.name.endsWith("!")

  def emptyPredicateList = ImplPredicateList(conditions.list.map((_) => Set[FunctionProperty]()))

  def implPredicateMap: ImplPredicateMap = {
    ImplPredicateMap(parameters.zip(conditions.list).toMap)
  }

  def propertiesForParameter(parameter: String): Set[FunctionProperty] = {
    this.conditions.list(this.parameters.indexOf(parameter))
  }

  def alphaConvert(newParameterNames: List[String]): ImplLhs = {
    ImplLhs(name, newParameterNames, conditions)
  }
}

object ImplLhs {
  def apply(name: String,
            parameters: List[String] = Nil,
            conditions: Option[ImplPredicateList] = None): ImplLhs = {
    ImplLhs(MethodName(name), parameters, conditions.getOrElse(ImplPredicateList(parameters.map((_) => Set[FunctionProperty]()))))
  }

  type FunctionProperty = String

  implicit object ImplLhsPartialOrdering extends PartialOrdering[ImplLhs] {
    def partialCompare(x: ImplLhs, y: ImplLhs): DominanceRelationship = {
      assert(x.name == y.name)

      PartialOrdering.fromSetOfDominanceRelationships(
        x.conditions.list.zip(y.conditions.list).map({case (xCond, yCond) => PartialOrdering.fromSetsOfProperties(xCond, yCond) })
      )
    }
  }
}

