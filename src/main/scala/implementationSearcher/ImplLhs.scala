package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty

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
}

object ImplLhs {
  def apply(name: String,
            parameters: List[String] = Nil,
            conditions: Option[ImplPredicateList] = None): ImplLhs = {
    ImplLhs(MethodName(name), parameters, conditions.getOrElse(ImplPredicateList(parameters.map((_) => Set[FunctionProperty]()))))
  }

  type FunctionProperty = String
}

case class ImplPredicate(parameterIdx: Int, property: String)

case class ImplPredicateList(list: List[Set[FunctionProperty]]) {
  def and(other: ImplPredicateList): ImplPredicateList = {
    assert(this.list.length == other.list.length, (this.list, other.list))

    ImplPredicateList(this.list.zip(other.list).map({ case ((x, y)) => x union y }))
  }

  def toNiceString(names: List[String]): String = {
    assert(names.length == list.length)

    list.zip(names).flatMap({ case ((set, name)) => set.toList.map(name + "." + _)}).mkString(", ")
  }

  def isEmpty: Boolean = list.forall(_.isEmpty)
}

case class ImplPredicateMap(map: Map[String, Set[FunctionProperty]]) {
  def and(other: ImplPredicateMap): ImplPredicateMap = {
    ImplPredicateMap(
      (map.keys ++ other.map.keys).toSet.map((parameterName: String) =>
        parameterName -> map.getOrElse(parameterName, Set()).union(other.map.getOrElse(parameterName, Set()))
      ).toMap
    )
  }

  def toList(parameters: List[String]): ImplPredicateList = {
    ImplPredicateList(parameters.map(map.getOrElse(_, Set())))
  }
}
