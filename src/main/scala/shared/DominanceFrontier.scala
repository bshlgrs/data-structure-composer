package shared

/**
  * Created by buck on 10/8/16.
  */
case class DominanceFrontier[A] private (items: Set[A], ordering: PartialOrdering[A]) {
  assert(items.forall((x: A) => items.forall((y: A) => ! ordering.partialCompare(x, y).oneStrictlyDominates)))

  def add(newItem: A): DominanceFrontier[A] = {
    // find all the items not strictly dominated by newItem
    if (items.exists((item) => ordering.partialCompare(item, newItem) == LeftStrictlyDominates)) {
      this
    } else {
      val undominatedItems = items.filter((item) => ordering.partialCompare(item, newItem) != RightStrictlyDominates)

      new DominanceFrontier(undominatedItems ++ Set(newItem), ordering)
    }
  }

  def partialCompareToItem(otherItem: A): DominanceRelationship = {
    items.map((item) => ordering.partialCompare(item, otherItem)).reduce(_.infimum(_))
  }

  def ++(other: DominanceFrontier[A]): DominanceFrontier[A] = {
    items.foldLeft(other)((frontier, item) => frontier.add(item))
  }

  override def toString: String = {
    s"""
       |DominanceFrontier {
       |${items.map("   " + _.toString).mkString("\n")}
       |}
     """.stripMargin
  }

  def filter(f: A => Boolean) = {
    new DominanceFrontier[A](items.filter(f), ordering)
  }
}

object DominanceFrontier {
  def fromSet[A: PartialOrdering](set: Set[A]): DominanceFrontier[A] = {
    set.foldLeft(empty[A])((frontier, item) => frontier.add(item))
  }

  def empty[A: PartialOrdering] = new DominanceFrontier[A](Set[A](), implicitly[PartialOrdering[A]])

  def emptyWithOrder[A](ordering: PartialOrdering[A]) = new DominanceFrontier[A](Set[A](), ordering)

  def fromSetWithOrder[A](set: Set[A], ordering: PartialOrdering[A]) =
    set.foldLeft(emptyWithOrder(ordering))((frontier, item) => frontier.add(item))
}
