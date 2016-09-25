package shared

/**
  * Created by buck on 9/25/16.
  */
object Utils {
  def cartesianProducts[A](list: List[Set[A]]): Set[List[A]] = list match {
    case Nil => Set(Nil)
    case x::xs =>
      for {
        y <- x
        z <- cartesianProducts(xs)
      } yield y :: z
  }
}
