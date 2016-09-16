package implementationSearcher

import shared.{ConstantTime, BigOLiteral}
import parsers.MainParser.implRhs

/**
  * Created by buck on 7/25/16.
  *
  */
case class ImplRhs(constant: BigOLiteral, costs: Map[MethodExpr, BigOLiteral] = Map()) {
  def +(other: ImplRhs): ImplRhs = {
    val combinedCosts = costs.foldLeft(other.costs) {
      case (currCosts: Map[MethodExpr, BigOLiteral], (methodExpr: MethodExpr, bigOLiteral: BigOLiteral)) => {
        currCosts + (methodExpr -> (currCosts.getOrElse(methodExpr, ConstantTime) + bigOLiteral))
      }
    }
    ImplRhs(this.constant + other.constant, combinedCosts)
  }

  def *(k: BigOLiteral): ImplRhs = {
    ImplRhs(this.constant * k, costs.mapValues(_ * k))
  }

  override def toString: String = {
    lazy val variableCostString = costs
      .toList
      .map({ case (m: MethodExpr, c: BigOLiteral) => if (c == ConstantTime) m.toString else s"${c.toShortString} * $m"})
      .mkString(" + ")


    (constant, costs.size) match {
      case (_, 0) => constant.toShortString
      case (ConstantTime, _) => variableCostString
      case (_, _) => variableCostString + " + " + constant.toShortString
    }
  }
}

object ImplRhs {
  def apply(str: String): ImplRhs = {
    implRhs.parse(str).get.value
  }
}
