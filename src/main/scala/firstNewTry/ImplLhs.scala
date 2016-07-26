package firstNewTry

/**
  * Created by buck on 5/17/16.
  */
case class ImplLhs(methodName: String,
                   parameters: List[String],
                   predicates: List[ImplPredicate]) {

  assert(!methodName.contains(" "), "There was a space in your method name! :(")

  override def toString = {
    val parametersString = if (parameters.nonEmpty) s"[${parameters.mkString(",")}]" else ""
    val predicateString = if (predicates.nonEmpty) s" if ${predicates.map(_.toString).mkString(", ")}" else ""
    s"$methodName$parametersString$predicateString"
  }
}
