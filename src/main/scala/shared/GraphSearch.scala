package shared

/**
  * Created by buck on 11/6/16.
  */
object GraphSearch {
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
}
