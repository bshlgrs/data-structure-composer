package implementationSearcher

import parsers.MainParser

/**
  * Created by buck on 10/23/16.
  */
case class ImplDeclaration(parameters: List[MethodName]) {

}

object ImplDeclaration {
  def parseMany(strings: String*): (Set[Impl], Map[MethodName, ImplDeclaration]) = {
    val tuples: Set[(Impl, ImplDeclaration)] =
      strings.toSet.map((x: String) => MainParser.nakedImpl.parse(x).get.value)

    val impls = tuples.map(_._1)
    val decls = tuples.map({case (impl, decl) => impl.lhs.name -> decl }).toMap

    (impls, decls)
  }

  def parseManyFromLhses(strings: String*): Map[MethodName, ImplDeclaration] = {
    val tuples: Set[(ImplLhs, ImplDeclaration)] =
      strings.toSet.map((x: String) => MainParser.nakedImplLhs.parse(x).get.value)

    tuples.map({case (lhs, decl) => lhs.name -> decl }).toMap
  }
}
