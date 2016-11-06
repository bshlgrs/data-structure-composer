package implementationSearcher

import implementationSearcher.ImplLhs.FunctionProperty
import parsers.MainParser
import shared.{DominanceRelationship, PartialOrdering}

case class DataStructure(parameters: List[MethodName],
                         conditions: ImplPredicateMap,
                         impls: Set[Impl],
                         extensionOf: Set[String]) {
  def isSimple: Boolean = parameters.isEmpty

//  def searchResult: UnfreeImplSet = UnfreeImplSet.fromSetOfUnfreeImpls(impls)

  def readMethods: Set[Impl] = {
    impls.filterNot(_.lhs.name.isMutating)
  }

  def writeMethods: Set[Impl] = {
    impls.filter(_.lhs.name.isMutating)
  }

  def namedParameters(sourceName: String): Set[BoundMethodName] = {
    parameters.map((x) => BoundMethodName(x.name, sourceName)).toSet
  }

//  def partialCompareWriteMethods(other: DataStructure): DominanceRelationship = {
//      (this.writeMethods.groupBy ++ other.writeMethods.keys).map((blah: MethodExpr) =>
//        (left.get(blah), right.get(blah)) match {
//          case (Some(x), Some(y)) => implicitly[PartialOrdering[B]].partialCompare(x, y)
//          case (Some(x), None) => LeftStrictlyDominates
//          case (None, Some(y)) => RightStrictlyDominates
//          case (None, None) => BothDominate
//        }
//      ).foldLeft(BothDominate: DominanceRelationship)(_ infimum _)
//  }
}


object DataStructure {

  def apply(string: String, decls: ImplLibrary.Decls): DataStructure = {
    val (ImplLhs(MethodName(dsName), dsConditions), ImplDeclaration(dsParameters), extentionOf, impls) = MainParser.nakedDataStructure.parse(string).get.value
    build(dsName, dsParameters, extentionOf, dsConditions, impls, decls)
  }

  def build(name: String,
            parameters: List[MethodName],
            extensionOf: Set[String],
            conditions: ImplPredicateMap,
            impls: Set[(Impl, ImplDeclaration)],
            decls: ImplLibrary.Decls): DataStructure = {
    val translatedImpls: Set[Impl] = impls.map({case (impl, implDecl) => {
      val implName = impl.lhs.name
      assert(decls.contains(implName), s"In the data structure $name, you define a method $implName, which doesn't exist")

      implDecl.parameters.zip(decls(implName).parameters).foldLeft(impl) ({ case (i: Impl, (nameHere: MethodName, declName: MethodName)) => {
        if (parameters.contains(nameHere)) {
          // rename nameHere to declName, add the dsConditions for that name to the impl conditions
          val newConditionsForThisParameter = i.lhs.conditions.get(nameHere) ++ conditions.get(nameHere)
          val newConditions = ImplPredicateMap(i.lhs.conditions.map - nameHere ++ Map(declName -> newConditionsForThisParameter))
          val newRhs = i.rhs.mapKeys((expr: MethodExpr) => if (expr.name == nameHere) expr.copy(name = declName) else expr)
          Impl(ImplLhs(implName, newConditions), newRhs)
        }
        else {
          assert(nameHere == declName, s"In the data structure $name, you have the impl $impl, which uses the parameter $nameHere, " +
            s"which is neither the normal parameter for that method nor a parameter of the data structure")

          i
        }
      }})
    }})

    DataStructure(parameters, conditions, translatedImpls, extensionOf)
  }
}
