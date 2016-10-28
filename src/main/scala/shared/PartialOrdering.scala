package shared

/**
  * Created by buck on 9/25/16.
  */
trait PartialOrdering[A] extends Ordering[A] {
  def partialCompare(lhs: A, rhs: A): DominanceRelationship

  def compare(lhs: A, rhs: A) = partialCompare(lhs, rhs) match {
    case LeftStrictlyDominates => -1
    case RightStrictlyDominates => 1
    case BothDominate => 0
    case NeitherDominates => 0
  }
}

object PartialOrdering {
  def fromSeqOfOrderedThings[A: PartialOrdering](set: Set[(A, A)]): DominanceRelationship = {
    fromSetOfDominanceRelationships(set.map({case (x, y) => implicitly[PartialOrdering[A]].partialCompare(x, y) }))
  }

  def fromSetsOfProperties[A](lhs: Set[A], rhs: Set[A]): DominanceRelationship = {
    DominanceRelationship.fromTwoBools(rhs.forall((x) => lhs.contains(x)), lhs.forall((x) => rhs.contains(x)))
  }

  def fromSetOfDominanceRelationships(set: Iterable[DominanceRelationship]): DominanceRelationship = {
    set.reduceOption(_.infimum(_)).getOrElse(BothDominate)
  }
}


