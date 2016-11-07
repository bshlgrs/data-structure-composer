package implementationSearcher

import shared.{DominanceRelationship, PartialOrdering}

/**
  * Created by buck on 11/6/16.
  */

// you can have multiple sources if you're eg an insert method
case class SingleBoundSource(template: Impl, materials: Set[Impl]) {

}

case class BoundSource(boundSources: Set[SingleBoundSource]) {
  def +(other: BoundSource): BoundSource = BoundSource(boundSources ++ other.boundSources)
}

case class BoundImpl(impl: Impl, boundSource: BoundSource)

case class BoundUnnamedImpl(impl: UnnamedImpl, boundSource: BoundSource) {
  def withName(name: MethodName) = BoundImpl(impl.withName(name), boundSource)
}

object BoundSource {
  def build(items: SingleBoundSource *): BoundSource = new BoundSource(items.toSet)
}

object BoundImpl {
  implicit def toImplFromBound(boundImpl: BoundImpl): Impl = boundImpl.impl

  implicit def toUnnamedImplFromBound(boundUnnamedImpl: BoundUnnamedImpl): UnnamedImpl =
    boundUnnamedImpl.impl

  implicit def toUnnamedBoundImplFromBoundImpl(boundImpl: BoundImpl): BoundUnnamedImpl =
    BoundUnnamedImpl(boundImpl.unnamed, boundImpl.boundSource)
}

object BoundUnnamedImpl {
  implicit object BoundUnnamedImplPartialOrdering extends PartialOrdering[BoundUnnamedImpl] {
    def partialCompare(x: BoundUnnamedImpl, y: BoundUnnamedImpl): DominanceRelationship = {
      implicitly[PartialOrdering[UnnamedImpl]].partialCompare(x.impl, y.impl)
    }
  }
}
