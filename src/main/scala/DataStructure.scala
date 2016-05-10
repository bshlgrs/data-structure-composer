/**
  * Created by buck on 5/8/16.
  */
case class DataStructure(name: String,
                         parameters: List[String],
                         predicates: List[ImplementationPredicate],
                         implementations: List[Implementation]) {
  override def toString: String = {
    val parameterString = if (parameters.nonEmpty) s"[${parameters.mkString(",")}]" else ""
    val predicateString = if (predicates.nonEmpty) s" if ${predicates.map(_.toString).mkString(", ")}" else ""
    s"$name$parameterString$predicateString {\n${implementations.map("    " + _.toString).mkString("\n")}\n}"
  }
}
