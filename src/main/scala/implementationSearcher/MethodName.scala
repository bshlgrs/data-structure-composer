package implementationSearcher

/**
  * Created by buck on 7/25/16.
  */
case class MethodName(name: String) {
  def isMutating = name.last == '!'

  assert(name.toList.forall((x: Char) => x.isLetter || x == '!'))

  override def toString: String = name
}

class BoundMethodName(methodName: String, source: String) extends MethodName(s"$source.$methodName")
object BoundMethodName {
  def apply(methodName: String, source: String) = new BoundMethodName(methodName, source)
}

object MethodName {
  implicit def fromString(s: String): MethodName = MethodName(s)
}
