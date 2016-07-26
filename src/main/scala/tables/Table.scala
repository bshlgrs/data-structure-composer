package tables

import scala.collection.mutable

/**
  * Created by buck on 7/20/16.
  */
case class Table[Item](contents: List[Item]) {
  def insert(item: Item): Table[Item] = {
    Table(contents :+ item)
  }

  def sortBy[A](f: Item => A)(implicit ordering: Ordering[A]): Unit = {
    Table(contents.sortBy(f))
  }

  def take[A](n: Int) = {
    Table(contents.take(n))
  }
}
