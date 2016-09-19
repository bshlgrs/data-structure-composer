package implementationSearcher

import shared.{ConstantTime, BigOLiteral}

/**
  * Created by buck on 9/18/16.
  */
case class AffineBigOCombo[A](bias: BigOLiteral, weights: Map[A, BigOLiteral] = Map()) {
  def +(other: AffineBigOCombo[A]): AffineBigOCombo[A] = {
    val combinedCosts = weights.foldLeft(other.weights) {
      case (currCosts: Map[_, BigOLiteral], (methodExpr, bigOLiteral: BigOLiteral)) => {
        currCosts + (methodExpr -> (currCosts.getOrElse(methodExpr, ConstantTime) + bigOLiteral))
      }
    }
    AffineBigOCombo(this.bias + other.bias, combinedCosts)
  }

  def +(other: BigOLiteral): AffineBigOCombo[A] = {
    AffineBigOCombo(bias + other, weights)
  }

  def *(c: BigOLiteral): AffineBigOCombo[A] = {
    AffineBigOCombo(c * bias, weights.mapValues(_ * c))
  }

  def addPair(key: A, value: BigOLiteral): AffineBigOCombo[A] = {
    this.copy(weights = weights.updated(key, this.weights.getOrElse(key, ConstantTime) + value))
  }

  override def toString: String = {
    lazy val variableCostString = weights
      .toList
      .map({ case (m, c: BigOLiteral) => if (c == ConstantTime) m.toString else s"${c.toShortString} * $m"})
      .mkString(" + ")


    (bias, weights.size) match {
      case (_, 0) => bias.toShortString
      case (ConstantTime, _) => variableCostString
      case (_, _) => variableCostString + " + " + bias.toShortString
    }
  }

  def mapKeys[B](f: A => B): AffineBigOCombo[B] = {
    weights.keys.foldLeft(AffineBigOCombo[B](this.bias, Map()))(
      (combo, key) => combo.addPair(f(key), weights(key)))
  }
}
