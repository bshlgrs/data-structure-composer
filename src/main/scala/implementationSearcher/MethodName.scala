package implementationSearcher

/**
  * Created by buck on 7/25/16.
  */
case class MethodName(name: String) {
  def isMutating = name.last == '!'

  override def toString: String = name
}
