package implementationSearcher

import shared._
import com.softwaremill.quicklens._
import org.scalactic.TypeCheckedTripleEquals._
import upickle.default
import upickle.Js

/**
  * Created by buck on 9/18/16.
  */
final class AffineBigOCombo[A](val bias: BigOLiteral, val weights: Map[A, BigOLiteral] = Map[A, BigOLiteral]()) {
  def +(other: AffineBigOCombo[A]): AffineBigOCombo[A] = {
    val combinedCosts = weights.foldLeft(other.weights) {
      case (currCosts: Map[_, BigOLiteral], (methodExpr, bigOLiteral: BigOLiteral)) => {
        currCosts + (methodExpr -> (currCosts.getOrElse(methodExpr, ZeroTime) + bigOLiteral))
      }
    }
    AffineBigOCombo(this.bias + other.bias, combinedCosts)
  }

  def copy(bias: BigOLiteral = bias, weights: Map[A, BigOLiteral] = weights) = AffineBigOCombo[A](bias, weights)

  def +(other: BigOLiteral): AffineBigOCombo[A] = {
    AffineBigOCombo(bias + other, weights)
  }

  val asStringForJson: String = toString

  def *(c: BigOLiteral): AffineBigOCombo[A] = {
    AffineBigOCombo(c * bias, weights.mapValues(_ * c))
  }

  def addPair(key: A, value: BigOLiteral): AffineBigOCombo[A] = {
    this.modify(_.weights).using(_.updated(key, this.weights.getOrElse(key, ZeroTime) + value))
  }

  def get(key: A): BigOLiteral = {
    weights.getOrElse(key, ZeroTime)
  }

  override def toString: String = {
    lazy val variableCostString = weights
      .toList
      .filter(_._2 != ZeroTime)
      .map({ case (m, c: BigOLiteral) => if (c === ConstantTime) m.toString else s"${c.toShortString} * $m"})
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

  def filterKeys(p: A => Boolean): AffineBigOCombo[A] = {
    this.modify(_.weights).using(_.filterKeys(p))
  }

  lazy val keys: Set[A] = weights.keys.toSet

  def partialCompare(other: AffineBigOCombo[A]): DominanceRelationship = {
    PartialOrdering.fromSetOfDominanceRelationships(
      (this.keys ++ other.keys).map((key) =>
        DominanceRelationship.fromTotalOrdering(other.get(key), this.get(key))
      ) ++ Set(DominanceRelationship.fromTotalOrdering(other.bias, this.bias))
    )
  }

  def substituteAllVariables(map: Map[A, BigOLiteral]): BigOLiteral = {
    // defaulting to constant time is actually not very legit here
    weights.keys.map((key) => map.getOrElse(key, ConstantTime)).foldLeft(bias)(_ + _)
  }

  lazy val asConstant: Option[BigOLiteral] = {
    if (weights.nonEmpty) {
      None
    } else {
      Some(bias)
    }
  }

  // todo: this might be stupid
  lazy val minCost: BigOLiteral = (bias +: (if (weights.values.isEmpty) Nil else List(weights.values.max))).max

  override def equals(other: Any) = other match {
    case otherBigOCombo: AffineBigOCombo[A] => otherBigOCombo.weights === weights && otherBigOCombo.bias === bias
    case _ => false
  }

  override def hashCode(): Int = AffineBigOCombo.hashCode() ^ weights.hashCode() << 1 ^ bias.hashCode() << 2
}

object AffineBigOCombo {
  trait AffineBigOPartialOrdering[A] extends shared.PartialOrdering[AffineBigOCombo[A]] {
    implicit def partialCompare(x: AffineBigOCombo[A], y: AffineBigOCombo[A]): DominanceRelationship = {
      x.partialCompare(y)
    }
  }

  def apply[A](bias: BigOLiteral, weights: Map[A, BigOLiteral] = Map()): AffineBigOCombo[A] = {
    new AffineBigOCombo(bias, weights.filter(_._2 != ZeroTime))
  }

  def unapply[A](arg: AffineBigOCombo[A]): Option[(BigOLiteral, Map[A, BigOLiteral])] = Some((arg.bias, arg.weights))

  implicit def thing2Writer[A] = upickle.default.Writer[AffineBigOCombo[A]]{
    case t: AffineBigOCombo[A] => Js.Str(t.toString)
  }


}
