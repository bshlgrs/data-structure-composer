package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import parsers.MainParser
import shared.{DominanceRelationship, PartialOrdering}

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
}

object ImplLhs {
  def parse(string: String) = {
    MainParser.nakedImplLhs.parse(string).get.value
  }

  type FunctionProperty = String


  // A dominates B if A can be used to implement B
  implicit object ImplLhsPartialOrdering extends PartialOrdering[ImplLhs] {
    def partialCompare(x: ImplLhs, y: ImplLhs): DominanceRelationship = {
      assert(x.name == y.name)

      PartialOrdering.fromSetOfDominanceRelationships(
        x.conditions.list.zip(y.conditions.list).map({case (xCond, yCond) => PartialOrdering.fromSetsOfProperties(xCond, yCond) })
      ).flip
    }
  }
}

