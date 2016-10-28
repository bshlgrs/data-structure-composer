package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import parsers.MainParser

/**
  * Created by buck on 7/31/16.
  */
// something like `unorderedEach[f]` or `insertAtEnd!` or `maxBy[func{commutative} <- f * n]`
case class MethodExpr(name: MethodName, args: List[FunctionExpr]) {
  override def toString: String = {
    val argsString = if (args.isEmpty) "" else s"[${args.map(_.toString).mkString(", ")}]"
    name.name + argsString
  }

  def getNames: Set[MethodName] = Set(name) ++ args.flatMap(_.getNames)

  def alphaConvert(map: Map[MethodName, MethodName]) = {
    MethodExpr(map.getOrElse(name, name), args.map(_.alphaConvert(map)))
  }
}

object MethodExpr {
  def apply(name: String, args: List[FunctionExpr] = Nil): MethodExpr = {

    MethodExpr(MethodName(name), args)
  }

  def parse(string: String): MethodExpr = {
    MainParser.justMethodExpr.parse(string).get.value
  }
}
