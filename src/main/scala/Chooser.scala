import scala.collection.mutable
import scala.math.Ordering.Implicits._

/**
  * Created by buck on 5/9/16.
  */
object Chooser {
  def main (args: Array[String]) {
    println(Chooser.chooseImplementationsForSingleDataStructure(LibraryLoader.library.structureWithName("VectorList").get, LibraryLoader.library)._2)
  }

  def chooseImplementationsForSingleDataStructure(structure: DataStructure,
                                                  library: Library): (Map[String, ImplementationInstantiation], Map[String, BigOExpression]) = {
    assert(structure.isSuperSimple)

    val queue = mutable.PriorityQueue[(BigOExpression, ImplementationInstantiation)]()(Ordering.by((x: (BigOExpression, ImplementationInstantiation)) => x._1).reverse)

    val thisDataStructure = DataStructureInstantiation(structure, Map())

    val chosenImplementations = mutable.Map[String, ImplementationInstantiation]()
    val chosenCosts = mutable.Map[String, BigOExpression]()

    for (impl <- structure.implementations.filter(_.isSuperSimple)) {
      impl.timeOnOwn match {
        case Some(time) => queue.enqueue((time, ImplementationInstantiation(impl, Some(thisDataStructure))))
        case None => {}
      }
    }

    while (queue.nonEmpty) {
      val (time, impl) = queue.dequeue()

      if (!chosenImplementations.contains(impl.methodName)) {
        chosenImplementations(impl.methodName) = impl
        chosenCosts(impl.methodName) = time

        for (impl <- library.implementationsWhichUse(impl.methodName) ) {
          impl.timeWith(chosenCosts.toMap) match {
            case Some(otherTime) => queue.enqueue((otherTime, ImplementationInstantiation(impl, None)))
            case None => {}
          }
        }
      }
    }

    (chosenImplementations.toMap, chosenCosts.toMap)
  }
}

