package shared

import implementationSearcher.MethodName

/**
  * Created by buck on 9/25/16.
  */


sealed abstract class DominanceRelationship {
  def infimum(other: DominanceRelationship) = (this, other) match {
    case (x, y) if x == y => x
    case (NeitherDominates, _) => NeitherDominates
    case (_, NeitherDominates) => NeitherDominates
    case (BothDominate, y) => y
    case (x, BothDominate) => x
    case (_, _) => NeitherDominates
  }

  def flip = this match {
    case LeftStrictlyDominates => RightStrictlyDominates
    case RightStrictlyDominates => LeftStrictlyDominates
    case x => x
  }

  def orIfTied(other: DominanceRelationship): DominanceRelationship = (this, other) match {
    case (BothDominate, _) => other
    case (NeitherDominates, _) => NeitherDominates
    case (LeftStrictlyDominates, _) => LeftStrictlyDominates
    case (RightStrictlyDominates, _) => RightStrictlyDominates
  }

  def leftDominates: Boolean = this == BothDominate || this == LeftStrictlyDominates
  def rightDominates: Boolean = this == BothDominate || this == RightStrictlyDominates
  def oneStrictlyDominates: Boolean = this == LeftStrictlyDominates || this == RightStrictlyDominates
}

object BothDominate extends DominanceRelationship
object LeftStrictlyDominates extends DominanceRelationship
object RightStrictlyDominates extends DominanceRelationship
object NeitherDominates extends DominanceRelationship

object DominanceRelationship {
  def fromTwoBools(leftGeqRight: Boolean, rightGeqLeft: Boolean): DominanceRelationship = (leftGeqRight, rightGeqLeft) match {
    case (true, true) => BothDominate
    case (true, false) => LeftStrictlyDominates
    case (false, true) => RightStrictlyDominates
    case (false, false) => NeitherDominates
  }

  def fromTotalOrdering[A <: Ordered[A]](lhs: A, rhs: A): DominanceRelationship = {
    if (lhs > rhs) {
      LeftStrictlyDominates
    } else if (lhs == rhs) {
      BothDominate
    } else {
      RightStrictlyDominates
    }
  }

//  def fromTwoMaps[A, B <: PartialOrdering[B]](left: Map[A, B], right: Map[A, B]): DominanceRelationship = {
//    (left.keys ++ right.keys).map((blah: A) =>
//      (left.get(blah), right.get(blah)) match {
//        case (Some(x), Some(y)) => implicitly[PartialOrdering[B]].partialCompare(x, y)
//        case (Some(x), None) => LeftStrictlyDominates
//        case (None, Some(y)) => RightStrictlyDominates
//        case (None, None) => BothDominate
//      }
//    ).foldLeft(BothDominate: DominanceRelationship)(_ infimum _)
//  }

  trait DominanceRelationshipFromTotalOrdering[A <: Ordered[A]] extends PartialOrdering[A] {
    def partialCompare(x: A, y: A): DominanceRelationship = {
      fromTotalOrdering(x, y)
    }
  }
}



//  trait AffineBigOPartialOrdering[A] extends shared.PartialOrdering[AffineBigOCombo[A]] {
//    def partialCompare(x: AffineBigOCombo[A], y: AffineBigOCombo[A]): DominanceRelationship = {
//      PartialOrdering.fromSetOfDominanceRelationships(
//        (x.keys ++ y.keys).map((key) =>
//          DominanceRelationship.fromTotalOrdering(y.get(key), x.get(key))
//        )
//      )
//    }
//  }
