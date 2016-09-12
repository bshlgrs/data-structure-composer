package implementationSearcher

/**
  * Created by buck on 7/31/16.
  */

sealed abstract class Dominance {
  def infimum(other: Dominance) = (this, other) match {
    case (x, y) if x == y => x
    case (Neither, _) => Neither
    case (_, Neither) => Neither
    case (Both, y) => y
    case (x, Both) => x
    case (_, _) => Neither
  }

  def firstDominates: Boolean = this == Both || this == First

  def secondDominates: Boolean = this == Both || this == Second
}

case object First extends Dominance
case object Second extends Dominance
case object Neither extends Dominance
case object Both extends Dominance

object Dominance {
  def fromTwoBools(aDominatesB: Boolean, bDominatesA: Boolean): Dominance = (aDominatesB, bDominatesA) match {
    case (true, true) => Both
    case (true, false) => First
    case (false, true) => Second
    case (false, false) => Neither
  }

  def fromSeq(seq: Seq[Dominance]) = seq.reduceOption(_ infimum _)

  def fromSeqOfOrderedThings[A](list: Seq[(A, A)])(implicit ord: Ordering[A]): Dominance = {
    fromSeq(list.map((x) => fromTwoBools(ord.gteq(x._1, x._2), ord.gteq(x._2, x._1)))).getOrElse(Both)
  }
}

trait DominanceFunction[A] {
  def apply(x: A, y: A): Dominance
}

