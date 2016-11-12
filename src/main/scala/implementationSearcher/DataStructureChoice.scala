package implementationSearcher

import scala.util.Try
import shared._
import org.scalactic.TypeCheckedTripleEquals._

/**
  * Created by buck on 10/10/16.
  */

case class DataStructureChoice(structureWriteMethods: Map[DataStructure, UnfreeImplSet],
                               readMethods: UnfreeImplSet,
                               adt: AbstractDataType,
                               freeImpls: Set[FreeImpl],
                               library: ImplLibrary) {

  readMethods.allImpls.foreach((i) => {
    val BoundSource(template, materials) = i.boundSource
    assert(freeImpls.exists(_.impl === template))
    materials.foreach((j) => {
      readMethods.getMatchingImpl(j).isDefined
    })
  })

  lazy val overallTimeForAdt: Option[BigOLiteral] = {
//    assert(results.keys.forall(_.args.isEmpty),
//       s"While calculating the overall time for an ADT on the data structure choice $this, " +
//         s"I noticed that some of your results ")

    if (adt.methods.keys.forall((x) => resultTimes.contains(x))) {
      Some(adt.methods.keys.map((methodName) => resultTimes(methodName) * adt.methods(methodName)).reduce(_ + _))
    } else {
      None
    }
  }

  lazy val results = adt.methods.keySet.flatMap((methodExpr: MethodExpr) => {
    // TODO: let this be a proper dominance frontier
    fullUnfreeImplSet.implsWhichMatchMethodExpr(methodExpr, ParameterList.empty, library.decls)
      .headOption.map((x) => methodExpr -> x._2)
  }).toMap

  lazy val resultTimes: Map[MethodExpr, BigOLiteral] = {
    results.mapValues(_.mapKeys(_.getAsNakedName).substituteAllVariables(adt.parameters))
  }

  lazy val structures: Set[DataStructure] = structureWriteMethods.keys.toSet

  lazy val writeMethods = structureWriteMethods.values.reduce(_ product _)

  def structureNames: Set[String] = structureWriteMethods.keys.map(_.name).toSet

  val freeImplSourceMap: Map[String, FreeImplSource] = freeImpls.map((x) => x.impl.toString -> x.freeImplSource).toMap

  lazy val fullUnfreeImplSet: UnfreeImplSet = readMethods.addImpls(writeMethods.allImpls)

  lazy val jsBoundHashCodeMap: Map[Int, BoundImpl] = readMethods.allImpls.map((x) => x.impl.hashCode() -> x).toMap
  lazy val jsFreeHashCodeMap: Map[Int, FreeImpl] = freeImpls.map((x) => x.impl.hashCode() -> x).toMap

  // todo: make things output themselves as strings so that it's easy to use some kind
  // of topological sort in the front end to explain everything.
  lazy val implStrings: Map[String, BoundImpl] = readMethods.allImpls.map((x) => x.impl.lhs.toString -> x).toMap


//  readMethods.allImpls.foreach((i) => {
//    val BoundSource(template, materials) = i.boundSource
//    assert(freeImpls.exists(_.impl === template))
//    materials.foreach((j) => {
//      readMethods.getMatchingImpl(j).isDefined
//    })
//  })

  lazy val frontendResult: Try[DataStructureChoiceFrontendResult] = {
    def getFrontendReadResult(i: BoundImpl, unfreeImplSet: UnfreeImplSet, localFreeImpls: Set[FreeImpl]): SingleMethodFrontendResult = {
      assert(unfreeImplSet.contains(i), {
        s"weird: the methods don't include $i"
      })
      val that = this
      val BoundSource(template, materials) = i.boundSource
      assert(i.boundSource.mbTemplate.isDefined, s"impl $i is fucked")

      SingleMethodFrontendResult(
        localFreeImpls.find(_.impl === template).getOrElse({
          throw new RuntimeException(s"oh dear, with i = $i")
        }),
        materials.map((j) => {
          val sourceImpl = unfreeImplSet.getMatchingImpl(j).getOrElse({
            throw new RuntimeException(
              s"In the bound source for $i, in the DSC $structureNames, " +
                s"there was no matching impl for $j")
          })

          getFrontendReadResult(sourceImpl, unfreeImplSet, localFreeImpls)
        })
      )
    }

    Try {
      val readMethodFrontendResults = adt.methods.keys.filter(_.name.isRead).map((x) => {
        readMethods.namedImplsWhichMatchMethodExpr(x, ParameterList.empty, library.decls).headOption match {
          case None => {
            throw new RuntimeException(s"There was no implementation for $x for the set $structureNames")
          }
          case Some(implBindResultTuple) => getFrontendReadResult(implBindResultTuple._3, readMethods, freeImpls)
        }
      })

      val writeMethodFrontendResults = adt.methods.keys.filter(_.name.isMutating).map((x) => {
        val structureMap: Map[String, SingleMethodFrontendResult] = structures.map((structure) => {
          val writeMethods = structureWriteMethods(structure)
          structure.name -> (writeMethods.namedImplsWhichMatchMethodExpr(x, ParameterList.empty, library.decls).headOption match {
            case None =>
              throw new RuntimeException(s"There was no implementation for $x for the set $structureNames")
            case Some(implBindResultTuple) => getFrontendReadResult(
              implBindResultTuple._3,
              writeMethods,
              freeImpls ++ structure.freeImpls)
          })
        }).toMap

        WriteMethodFrontendResult(x.name.name, structureMap)
      }).toSet

      DataStructureChoiceFrontendResult(readMethodFrontendResults.toSet, writeMethodFrontendResults)
    }
  }
}

object DataStructureChoice {
  class DataStructureChoicePartialOrdering(library: ImplLibrary)
    extends shared.PartialOrdering[DataStructureChoice] {
    def partialCompare(x: DataStructureChoice, y: DataStructureChoice): DominanceRelationship = {
      val timeComparison = PartialOrdering.fromSetOfDominanceRelationships(
        (x.results.keys ++ y.results.keys).map((key) => (x.results.get(key), y.results.get(key)) match {
          case (Some(xRes), Some(yRes)) => xRes.partialCompare(yRes)
          case (Some(_), None) => LeftStrictlyDominates
          case (None, Some(_)) => RightStrictlyDominates
          case (None, None) => BothDominate
        }).toSet
      )

      val simplicityComparison =
        PartialOrdering.fromSetsOfProperties(x.structures, y.structures).flip.neitherToBoth
          .orIfTied(library.partialCompareSetFromExtensionRelations(x.structures, y.structures))

      timeComparison.orIfTied(simplicityComparison)
    }
  }

  def build(choices: Map[DataStructure, UnfreeImplSet],
            times: UnfreeImplSet,
            adt: AbstractDataType,
            library: ImplLibrary): DataStructureChoice = {
    DataStructureChoice(choices, times, adt, library.impls ++ choices.keys.flatMap(_.freeImpls), library)
  }
}

case class DataStructureChoiceFrontendResult(readMethods: Set[SingleMethodFrontendResult], writeMethods: Set[WriteMethodFrontendResult]) {

}

case class SingleMethodFrontendResult(template: FreeImpl, materials: Set[SingleMethodFrontendResult]) {

}

case class WriteMethodFrontendResult(methodName: MethodName, impls: Map[String, SingleMethodFrontendResult])
