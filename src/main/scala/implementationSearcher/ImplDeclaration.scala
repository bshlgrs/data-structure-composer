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
      strings.toSet.map((x: String) => MainParser.impl.parse(x).get.value)

    val impls = tuples.map(_._1)
    val decls = tuples.map({case (impl, decl) => impl.lhs.name -> decl }).toMap

    (impls, decls)
  }
}
