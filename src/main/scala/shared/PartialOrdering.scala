package shared

/**
  * Created by buck on 9/25/16.
  */
trait PartialOrdering[A] {
  def partialCompare(lhs: A, rhs: A): DominanceRelationship
}

object PartialOrdering {
  def fromSeqOfOrderedThings[A: PartialOrdering](set: Set[(A, A)]): DominanceRelationship = {
    fromSetOfDominanceRelationships(set.map({case (x, y) => implicitly[PartialOrdering[A]].partialCompare(x, y) }))
  }

  def fromSetsOfProperties[A](lhs: Set[A], rhs: Set[A]): DominanceRelationship = {
    DominanceRelationship.fromTwoBools(rhs.forall((x) => lhs.contains(x)), lhs.forall((x) => rhs.contains(x)))
  }

  def fromSetOfDominanceRelationships(set: Iterable[DominanceRelationship]): DominanceRelationship = {
    set.reduceOption(_.infimum(_)).getOrElse(NeitherDominates)
  }
}

class DominanceFrontier[A: PartialOrdering](val items: Set[A]) {
  assert(items.forall((x: A) => items.forall((y: A) => implicitly[PartialOrdering[A]].partialCompare(x, y) != BothDominate)))

  def add(newItem: A): DominanceFrontier[A] = {
    // find all the items not strictly dominated by newItem
    if (items.exists((item) => implicitly[PartialOrdering[A]].partialCompare(item, newItem) == LeftStrictlyDominates)) {
      this
    } else {
      val undominatedItems = items.filter((item) => implicitly[PartialOrdering[A]].partialCompare(item, newItem) != RightStrictlyDominates)

      new DominanceFrontier(undominatedItems ++ Set(newItem))
    }
  }

  def partialCompareToItem(otherItem: A): DominanceRelationship = {
    items.map((item) => implicitly[PartialOrdering[A]].partialCompare(item, otherItem)).reduce(_.infimum(_))
  }

  def union(other: DominanceFrontier[A]): DominanceFrontier[A] = {
    items.foldLeft(other)((frontier, item) => frontier.add(item))
  }
}

