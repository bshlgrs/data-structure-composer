package implementationSearcher

import scala.annotation.tailrec

/**
  * Created by buck on 10/31/16.
  */
case class ImplLibrary(impls: Set[Impl], decls: Map[MethodName, ImplDeclaration], structures: Map[String, DataStructure]) {

  // suppose we had x <- y and y <- z
  // then our arrows would be
  lazy val closuresOfForwardImplArrows: Map[MethodName, Set[MethodName]] = closureOfMap(forwardImplArrows)
  lazy val closuresOfBackwardImplArrows: Map[MethodName, Set[MethodName]] = closureOfMap(backwardImplArrows)

  def closureOfMap[A](map: Map[A, Set[A]]): Map[A, Set[A]] = {
    def dfs(start: A): Set[A] = {
      val visited = collection.mutable.Set(start)
      val frontier = collection.mutable.Stack(map(start).toSeq:_*)

      while (frontier.nonEmpty) {
        val item = frontier.pop()

        map(item).foreach((neighbor) => {
          if (!visited.contains(neighbor)) {
            visited.add(neighbor)
            frontier.push(neighbor)
          }
        })
      }

      visited.toSet
    }

    map.keys.map((k) => k -> dfs(k)).toMap
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
      .flatMap({ case (m: MethodName, s: Set[MethodName]) => s.map((m2) => m -> m2)})
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
  }
}
