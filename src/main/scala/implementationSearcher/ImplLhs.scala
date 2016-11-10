package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import implementationSearcher.ImplPredicateMap.ImplPredicateMapPartialOrdering
import parsers.MainParser
import shared.{NeitherDominates, DominanceRelationship, PartialOrdering}

/**
  * Created by buck on 7/25/16.
  */
case class ImplLhs(name: MethodName, conditions: ImplPredicateMap) {
  def addConditions(conditions: ImplPredicateMap) = {
    this.copy(conditions = this.conditions.and(conditions))
  }


  override def toString: String = {
    val conditionsString = if (conditions.isEmpty) "" else s"if ${conditions.toNiceString}"

    s"${name.name} $conditionsString"
  }

  def isMutating: Boolean = name.name.endsWith("!")


  def propertiesForParameter(parameter: MethodName): Set[FunctionProperty] = {
    this.conditions.get(parameter)
  }

  lazy val asString = toString
}

object ImplLhs {
  def parse(string: String) = {
    MainParser.nakedImplLhs.parse(string).get.value._1
  }

  type FunctionProperty = String


  // A dominates B if A can be used to implement B
  implicit object ImplLhsPartialOrdering extends PartialOrdering[ImplLhs] {
    def partialCompare(x: ImplLhs, y: ImplLhs): DominanceRelationship = {
      if (x.name == y.name)
        ImplPredicateMapPartialOrdering.partialCompare(x.conditions, y.conditions)
      else
        NeitherDominates
    }
  }
}

