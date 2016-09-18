package implementationSearcher

import shared.{ConstantTime, BigOLiteral}

/**
  * Created by buck on 9/18/16.
  */
case class AffineBigOCombo[A](k: BigOLiteral, m: Map[A, BigOLiteral] = Map()) {
  def +(other: AffineBigOCombo[A]): AffineBigOCombo[A] = {
    val combinedCosts = m.foldLeft(other.m) {
      case (currCosts: Map[_, BigOLiteral], (methodExpr, bigOLiteral: BigOLiteral)) => {
        currCosts + (methodExpr -> (currCosts.getOrElse(methodExpr, ConstantTime) + bigOLiteral))
      }
    }
    AffineBigOCombo(this.k + other.k, combinedCosts)
  }

  def +(other: BigOLiteral): AffineBigOCombo[A] = {
    AffineBigOCombo(k + other, m)
  }

  def *(c: BigOLiteral): AffineBigOCombo[A] = {
    AffineBigOCombo(c * k, m.mapValues(_ * c))
  }

  override def toString: String = {
    lazy val variableCostString = m
      .toList
      .map({ case (m, c: BigOLiteral) => if (c == ConstantTime) m.toString else s"${c.toShortString} * $m"})
      .mkString(" + ")


    (k, m.size) match {
      case (_, 0) => k.toShortString
      case (ConstantTime, _) => variableCostString
      case (_, _) => variableCostString + " + " + k.toShortString
    }
  }
}
