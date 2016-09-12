package secondNewTry

/**
  * Created by buck on 7/31/16.
  */
//case class DominanceFrontier[T](frontier: Set[T]) {
//  def map[U](f: T => U)(implicit dom: DominanceFunction[T]): DominanceFrontier[U] = {
//    DominanceFrontier.fromUncleanSet(frontier.map(f))
//  }
//
//  def flatMap[U](f: T => DominanceFrontier[U])
//                (implicit domT: DominanceFrontier[T], domU: DominanceFrontier[U]) = {
//    DominanceFrontier.fromUncleanSet(frontier.flatMap((x) => f(x).frontier))
//  }
//
//  def add(el: T)(implicit dom: DominanceFunction[T]): DominanceFrontier[T] = {
//    if (frontier.exists((x) => dom(x, el).firstDominates)) {
//      this
//    } else {
//      DominanceFrontier(frontier.filter((x) => dom(x, el) == Neither) ++ Set(el))
//    }
//  }
//}

//object DominanceFrontier {
//  def fromUncleanSet[T](set: Set[T]): DominanceFrontier[T] = set match {
//    case Set.empty => DominanceFrontier(Set.empty)
//    case ((x: T, xs: Set[T])) => DominanceFrontier(xs).add(x)
//  }
//}
