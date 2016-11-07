package implementationSearcher

import implementationSearcher.ImplLibrary.Decls
import shared._

import scala.annotation.tailrec

/**
  * Created by buck on 10/31/16.
  */
case class ImplLibrary(impls: Set[FreeImpl], decls: Decls, structures: Map[String, DataStructure]) {
  // suppose we had x <- y and y <- z
  // then our arrows would be
  lazy val closuresOfForwardImplArrows: Map[MethodName, Set[MethodName]] = GraphSearch.closureOfMap(forwardImplArrows)
  lazy val closuresOfBackwardImplArrows: Map[MethodName, Set[MethodName]] = GraphSearch.closureOfMap(backwardImplArrows)

  lazy val writeMethods: Set[FreeImpl] = impls.filter(_.lhs.isMutating)
  lazy val readMethods: Set[FreeImpl] = impls.filter(!_.lhs.isMutating)

  // map from MethodNames to all the things that can be used to implement them
  // eg, if we just had `reduce` and `getSum` here, our impls would have `getSum <- reduce[_]` in them,
  // so our map here would be Map(getSum -> reduce)
  lazy val backwardImplArrows: Map[MethodName, Set[MethodName]] = {
    (impls ++ structures.flatMap(_._2.freeImpls)).groupBy(_.impl.lhs.name).map({ case (m: MethodName, s: Set[FreeImpl]) =>
      m -> s.flatMap(_.impl.rhs.weights.keys.flatMap(_.getNames))
    })
  }

  // our map here would be Map(reduce -> getSum)
  lazy val forwardImplArrows: Map[MethodName, Set[MethodName]] = {
    backwardImplArrows
      .toList
      .flatMap({ case (m: MethodName, s: Set[MethodName]) => s.map((m2) => m2 -> m)})
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
  }

  // suppose A extends B, and B extends C. Then we have A -> {A, B, C}, B -> {B, C}, C -> {C}
  lazy val closuresOfExtensionArrows: Map[String, Set[String]] =
    GraphSearch.closureOfMap(structures.mapValues(_.extensionOf))

  def oneDsExtendsOther(x: DataStructure, y: DataStructure): Boolean = {
    closuresOfExtensionArrows(x.name).contains(y.name) || closuresOfExtensionArrows(y.name).contains(x.name)
  }

  def potentiallyRelevantDataStructures(adt: AbstractDataType) = {
    structures.valuesIterator.filter({ case structure: DataStructure => {
      // This structure is potentially relevant if there's any way for the read methods it contributes
      // to be useful to the implementation of the ADT's methods

      val adtReadMethods = adt.methods.map(_._1.name).toSet

      structure.readMethods.exists((i: FreeImpl) => {
        val methodNamesThisIsHelpfulFor = closuresOfForwardImplArrows(i.lhs.name)
        (adtReadMethods & methodNamesThisIsHelpfulFor).nonEmpty
      })
    }}).toSet
  }

  // If you compare Ost to AugmentedOst, the only reason to use AugmentedOst is if you need to use the
  // twoSidedIndexRangeQuery at some point. Otherwise, Ost is better.

  // This can be calculated by observing that Ost has strictly better write methods, and then finding the set of queries
  // which AugmentedOst is better than Ost at.

  // If there's a totally useless structure, it will never be used.

  // map from data structure names to all of the read methods which that data structure implements faster than every
  // data structure which dominates it on write times
  lazy val qualifyingReadMethodsForDataStructures: Map[String, Option[Set[MethodName]]] = {
//    structures.map({ case (structureName, structure) => {
//      val otherStructures: Iterable[DataStructure] = structures.filterKeys(_ != structureName)
//        .values
//        .filter((other) => structure.partialCompareWriteMethods(other).rightDominates)
//
////      otherStructures.flatMap((other) => {
////
////      })
//      ???
//    }}).toMap
    ???
  }

  lazy val dataStructureChoicePartialOrdering = new DataStructureChoice.DataStructureChoicePartialOrdering(this)

  def partialCompareSetFromExtensionRelations(x: Set[String], y: Set[String]): DominanceRelationship = {
    val list = for {
      xChoice <- x
      yChoice <- y
    } yield partialCompareFromExtensionRelation(xChoice, yChoice)

    list.reduce(_ supremum _)
  }

  def partialCompareFromExtensionRelation(x: String, y: String): DominanceRelationship = {
    if (x == y)
      NeitherDominates
    else if (closuresOfExtensionArrows(x).contains(y))
      RightStrictlyDominates
    else if (closuresOfExtensionArrows(y).contains(x))
      LeftStrictlyDominates
    else
      NeitherDominates
  }

  def isImplRelevantToAdt(impl: Impl, adt: AbstractDataType): Boolean = {
    closuresOfForwardImplArrows(impl.lhs.name)
      .exists((x) => adt.methods.keySet.map(_.name).contains(x))
  }
}

object ImplLibrary {
  type Decls = Map[MethodName, ImplDeclaration]
}
