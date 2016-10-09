package shared

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
      NeitherDominates
    } else {
      RightStrictlyDominates
    }
  }
}
