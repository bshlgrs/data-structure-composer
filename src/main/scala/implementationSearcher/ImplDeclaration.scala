package implementationSearcher

import parsers.MainParser

/**
  * Created by buck on 10/23/16.
  */
case class ImplDeclaration(parameters: List[MethodName]) {
}

object ImplDeclaration {
  lazy val empty = ImplDeclaration(Nil)

  def parseMany(strings: String*): (Set[FreeImpl], Map[MethodName, ImplDeclaration]) = {
    val tuples: Set[(FreeImpl, ImplDeclaration)] =
      strings.toSet.map((x: String) => MainParser.nakedImpl.parse(x).get.value).map((x) => FreeImpl.wrap(x._1) -> x._2)

    val impls = tuples.map(_._1)
    val decls = tuples.map({case (impl, decl) => impl.name -> decl }).toMap

    (impls, decls)
  }

  def parseManyFromLhses(strings: String*): Map[MethodName, ImplDeclaration] = {
    val tuples: Set[(ImplLhs, ImplDeclaration)] =
      strings.toSet.map((x: String) => MainParser.nakedImplLhs.parse(x).get.value)

    tuples.map({case (lhs, decl) => lhs.name -> decl }).toMap
  }
}
