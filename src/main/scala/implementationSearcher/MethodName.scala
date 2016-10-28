package implementationSearcher

/**
  * Created by buck on 7/25/16.
  */
case class MethodName(name: String) {
  def isMutating = name.last == '!'

  assert(name.toList.forall((x: Char) => x.isLetter || x == '!'))

  override def toString: String = name
}

object MethodName {
  implicit def fromString(s: String): MethodName = MethodName(s)
}
