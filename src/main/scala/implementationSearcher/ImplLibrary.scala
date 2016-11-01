package implementationSearcher

import implementationSearcher.ImplLibrary.Decls

import scala.annotation.tailrec

/**
  * Created by buck on 10/31/16.
  */
case class ImplLibrary(impls: Set[Impl], decls: Decls, structures: Map[String, DataStructure]) {
  // suppose we had x <- y and y <- z
  // then our arrows would be
  lazy val closuresOfForwardImplArrows: Map[MethodName, Set[MethodName]] = closureOfMap(forwardImplArrows)
  lazy val closuresOfBackwardImplArrows: Map[MethodName, Set[MethodName]] = closureOfMap(backwardImplArrows)

  def closureOfMap[A](map: Map[A, Set[A]]): Map[A, Set[A]] = {
    def dfs(start: A): Set[A] = {
      val visited = collection.mutable.Set[A](start)
      val frontier = collection.mutable.Stack(start)

      while (frontier.nonEmpty) {
        val item = frontier.pop()

        map.getOrElse(item, Set()).foreach((neighbor) => {
          if (!visited.contains(neighbor)) {
            visited.add(neighbor)
            frontier.push(neighbor)
          }
        })
      }

      visited.toSet
    }

    (map.keys ++ map.values.flatten.toSet).map((k) => k -> dfs(k)).toMap
  }

  // map from MethodNames to all the things that can be used to implement them
  // eg, if we just had `reduce` and `getSum` here, our impls would have `getSum <- reduce[_]` in them,
  // so our map here would be Map(getSum -> reduce)
  lazy val backwardImplArrows: Map[MethodName, Set[MethodName]] = {
    (impls ++ structures.flatMap(_._2.impls)).groupBy(_.lhs.name).map({ case (m: MethodName, s: Set[Impl]) =>
      m -> s.flatMap(_.rhs.weights.flatMap(_._1.getNames))
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

  def potentiallyRelevantDataStructures(adt: AbstractDataType) = {
    structures.filter({ case (name: String, structure: DataStructure) => {
      // This structure is potentially relevant if there's any way for the read methods it contributes
      // to be useful to the implementation of the ADT's methods

      val adtReadMethods = adt.methods.map(_._1.name).toSet

      structure.readMethods.exists((i: Impl) => {
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
}

object ImplLibrary {
  type Decls = Map[MethodName, ImplDeclaration]
}
