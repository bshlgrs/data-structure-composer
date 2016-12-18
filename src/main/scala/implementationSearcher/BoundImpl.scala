package implementationSearcher

import shared.{DominanceRelationship, PartialOrdering}

/**
  * Created by buck on 11/6/16.
  */

sealed abstract class AbstractBoundSource {
  def mbTemplate: Option[Impl]
  def materialSet: Set[Impl]
}

case class BoundSource(template: Impl, materials: Set[Impl]) extends AbstractBoundSource {
  def mbTemplate = Some(template)
  def materialSet: Set[Impl] = materials
}

case object EmptyBoundSource extends AbstractBoundSource {
  lazy val mbTemplate = None
  lazy val materialSet = Set[Impl]()
}

case class BoundImpl(impl: Impl, boundSource: AbstractBoundSource) {
  lazy val jsonValue: Map[String, Any] = Map("valueString" -> impl.toString)
}

case class BoundUnnamedImpl(impl: UnnamedImpl, boundSource: AbstractBoundSource) {
  def withName(name: MethodName) = BoundImpl(impl.withName(name), boundSource)
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
