package secondNewTry

/**
  * Created by buck on 7/25/16.
  */
case class MethodName(name: String) {
  def isMutating = name.last == '!'
}
