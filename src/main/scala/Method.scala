/**
  * Created by buck on 5/1/16.
  */
case class Method(name: String) {

}

object Methods {
  val methods = Set("getByIndex", "getFirst", "getNext").map(Method(_))
}