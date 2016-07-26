package firstNewTry

import scala.collection.mutable

/**
  * Created by buck on 5/9/16.
  */
object Chooser {
//  def main (args: Array[String]) {
//    println(Chooser.chooseImplementationsForSingleDataStructureSimply(LibraryLoader.library.structureWithName("VectorList").get, LibraryLoader.library)._2)
//  }

//  def chooseImplementationsForSingleDataStructureSimply(structure: DataStructure,
//                                                  library: Library): (Map[String, ImplInstantiation], Map[String, BigOLiteral]) = {
//    assert(structure.isSuperSimple)
//
//    val queue = mutable.PriorityQueue[(BigOLiteral, ImplInstantiation)]()(Ordering.by((x: (BigOLiteral, ImplInstantiation)) => x._1).reverse)
//
//    val thisDataStructure = DataStructureInstantiation(structure, Map())
//
//    val chosenImplementations = mutable.Map[String, ImplInstantiation]()
//    val chosenCosts = mutable.Map[String, BigOLiteral]()
//
//    for (impl <- structure.implementations.filter(_.parameters.length <= 1)) {
//      impl.timeOnOwn match {
//        case Some(time) => queue.enqueue((time, ImplInstantiation(impl, Some(thisDataStructure), Some(Nil))))
//        case None => {}
//      }
//    }
//
//    while (queue.nonEmpty) {
//      val (time, impl) = queue.dequeue()
//
//      if (!chosenImplementations.contains(impl.methodName)) {
//        chosenImplementations(impl.methodName) = impl
//        chosenCosts(impl.methodName) = time
//
//        for (impl <- library.implementationsWhichUse(impl.methodName) ) {
//          impl.timeWith(chosenCosts.toMap) match {
//            case Some(otherTime) => queue.enqueue((otherTime, ImplInstantiation(impl, None, None)))
//            case None => {}
//          }
//        }
//      }
//    }
//
//    (chosenImplementations.toMap, chosenCosts.toMap)
//  }
//
//  def chooseImplementationsForSingleDataStructure(structure: DataStructure,
//                                                  library: Library): (Map[String, ImplInstantiation], Map[String, BigOLiteral]) = {
//    assert(structure.isSuperSimple)
//
//    val queue = mutable.PriorityQueue[(BigOLiteral, ImplInstantiation)]()(Ordering.by((x: (BigOLiteral, ImplInstantiation)) => x._1).reverse)
//
//    val thisDataStructure = DataStructureInstantiation(structure, Map())
//
//    val chosenImplementations = mutable.Map[String, ImplInstantiation]()
//    val chosenCosts = mutable.Map[String, BigOLiteral]()
//
//    for (impl <- structure.implementations.filter(_.parameters.length <= 1)) {
//      impl.timeOnOwn match {
//        case Some(time) => queue.enqueue((time, ImplInstantiation(impl, Some(thisDataStructure), Some(Nil))))
//        case None => ()
//      }
//    }
//
//    while (queue.nonEmpty) {
//      val (time, impl) = queue.dequeue()
//
//      if (!chosenImplementations.contains(impl.methodName)) {
//        chosenImplementations(impl.methodName) = impl
//        chosenCosts(impl.methodName) = time
//
//        for (impl <- library.implementationsWhichUse(impl.methodName) ) {
//          impl.timeWith(chosenCosts.toMap) match {
//            case Some(otherTime) => queue.enqueue((otherTime, ImplInstantiation(impl, None, None)))
//            case None => ()
//          }
//        }
//      }
//    }
//
//    (chosenImplementations.toMap, chosenCosts.toMap)
//  }

}

