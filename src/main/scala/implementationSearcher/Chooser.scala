package implementationSearcher


import parsers.MainParser
import shared._

import scala.collection.mutable
import scala.io.Source

/**
  * Created by buck on 7/25/16.
  */
object Chooser {
  // x[f] if x.foo <- 1
  // y[g] <- x[g]
  // should infer
  // y[g] if g.foo <- 1
//  val testLibrary = Set(
//    Impl(ImplLhs("x", List("f"), Some(ImplPredicateList(List(Set("foo"))))), AffineBigOCombo[MethodExpr](LogTime)),
//    Impl(ImplLhs("y", List("g")),
//      AffineBigOCombo[MethodExpr](ConstantTime, Map(MethodExpr("x", List(NamedFunctionExpr("g"))) -> ConstantTime)))
//  )

  def getAllTimes(impls: Set[Impl], freeVariables: Set[MethodName], declarations: Map[MethodName, ImplDeclaration]): UnfreeImplSet = {
    val queue = mutable.Set[Impl]()

    var unfreeImplSet = UnfreeImplSet(Map(), freeVariables, declarations)


    queue ++= impls.filter(_.unboundCostTuples(unfreeImplSet).isEmpty)

    def queuePlusSelected: List[Impl] = queue.toList ++ unfreeImplSet.allImpls

    while (queue.nonEmpty) {
      val unfreeImpl: Impl = queue.minBy(_.rhs.minCost)

      queue.remove(unfreeImpl)

      if (unfreeImplSet.isOtherImplUseful(unfreeImpl)) {
        unfreeImplSet = unfreeImplSet.addImpl(unfreeImpl)



        for (otherImpl <- impls) {
          // So we have a random impl. Let's see if the unfreeImpl we just settled on is useful for that impl.
          // It's only useful if unfreeImpl's methodName is used by the rhs of the otherImpl (this condition is weaker than it could be)

          val otherImplMethodsUsed = otherImpl.getNames

          if (otherImplMethodsUsed.contains(unfreeImpl.lhs.name)) {
            val neighborUnfreeImpls = otherImpl.bindToAllOptions(unfreeImplSet)

            neighborUnfreeImpls.items.foreach((u: UnnamedImpl) =>
              if (unfreeImplSet.isOtherImplUseful(u.withName(otherImpl.lhs.name))) {
                val impl = u.withName(otherImpl.lhs.name)
                assert(impl.unboundCostTuples(unfreeImplSet).isEmpty)
                queue += impl
              }
            )
          }
        }
      }
    }

    unfreeImplSet
  }

  def getAllTimesForDataStructure(impls: Set[Impl], dataStructure: DataStructure, declarations: Map[MethodName, ImplDeclaration]) = {
    // todo: consider conditions
    getAllTimes(impls.union(dataStructure.impls), dataStructure.parameters.toSet, declarations)
  }


  def getRelevantTimesForDataStructures(impls: Set[Impl],
                                        structures: Set[DataStructure],
                                       decls: Map[MethodName, ImplDeclaration]): UnfreeImplSet = {
    val allProvidedReadImplementations: Set[Impl] = structures.flatMap(_.readMethods)

    val allFreeVariables = structures.flatMap(_.parameters)

    val bestReadImplementations: UnfreeImplSet = getAllTimes(allProvidedReadImplementations ++ impls, allFreeVariables, decls)


    val allWriteImplementations: Set[UnfreeImplSet] = structures.map((s) =>
      getAllTimes(s.writeMethods ++ bestReadImplementations.allImpls ++ impls, s.parameters.toSet, decls))

    val combinedWriteImplementations: UnfreeImplSet = allWriteImplementations.reduceOption(_.product(_)).getOrElse(UnfreeImplSet(Map(), allFreeVariables, decls))

    bestReadImplementations.addImpls(combinedWriteImplementations.allImpls)
  }


  def allParetoOptimalDataStructureCombosForAdt(impls: Set[Impl],
                                                structures: Map[String, DataStructure],
                                                decls: Map[MethodName, ImplDeclaration],
                                                adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    val results = structures.toSet.subsets().map((subset) => {
      subset -> getRelevantTimesForDataStructures(impls, subset.map(_._2), decls)
    }).toSet

    val choicesSet: Set[DataStructureChoice] = results.flatMap({ case (set: Set[(String, DataStructure)], sr: UnfreeImplSet) => {
      val methods = adt.methods.keys.map((methodExpr: MethodExpr) => {
        // TODO: let this be a proper dominance frontier
        methodExpr -> sr.implsWhichMatchMethodExpr(methodExpr, ParameterList.empty).headOption.map(_.withName(methodExpr.name))
      }).toMap

      if (methods.forall(_._2.isDefined))
        Set[DataStructureChoice](DataStructureChoice(set.map(_._1), methods.mapValues(_.get.rhs.mapKeys(_.getAsNakedName))))
      else
        Set[DataStructureChoice]()
    }})

    // A dominance frontier on choices, ranked by simplicity and also on the methods which the ADT cares about.
    DominanceFrontier.fromSet(choicesSet)
  }

  // this is the ultimate method
  def allMinTotalCostParetoOptimalDataStructureCombosForAdt(impls: Set[Impl],
                                                            structures: Map[String, DataStructure],
                                                            decls: Map[MethodName, ImplDeclaration],
                                                            adt: AbstractDataType): DominanceFrontier[DataStructureChoice] = {
    adt.methods.keys.foreach((x) => {
      assert(decls.contains(x.name),
        s"Your ADT referred to ${x.name}, which doesn't exist")
    })


    val frontier = allParetoOptimalDataStructureCombosForAdt(impls, structures, decls, adt)

    val bestTime = frontier.items.map(_.overallTimeForAdt(adt)).min

    frontier.filter(_.overallTimeForAdt(adt) == bestTime)
  }
}

