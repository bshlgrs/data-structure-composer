/**
  * Created by buck on 5/1/16.
  */
abstract class Implementation(method: Method, requirements: Map[Method, BigOExpression], internalTime: BigOExpression, parent: Option[DataStructure]) {
  def totalTimeFromImplementations(otherImplementations: Map[Method, Implementation]): Option[BigOExpression] = {
    if (requirements.size == 0)
      Some(internalTime)
    else
      requirements.keys.map(otherImplementations(_).totalTimeFromImplementations(otherImplementations)).reduce((xOpt, yOpt) => {
        (xOpt, yOpt) match {
          case (Some(x), Some(y)) => Some(x + y)
          case _ => None
        }
      }).map(_ + internalTime)
  }

  def totalTimeFromTimes(otherTimes: Map[Method, BigOExpression]): Option[BigOExpression] = {
    if (requirements.size == 0)
      Some(internalTime)
    else
      requirements.keys.map(otherTimes.get(_)).reduce((xOpt, yOpt) => {
        (xOpt, yOpt) match {
          case (Some(x), Some(y)) => Some(x + y)
          case _ => None
        }
      }).map(_ + internalTime)
  }
}

case class InternalImplementation(method: Method, requirements: Map[Method, BigOExpression] = Map(), time: BigOExpression, structure: DataStructure)
  extends Implementation(method, requirements, time, Some(structure))

case class UniversalImplementation(method: Method, requirements: Map[Method, BigOExpression], internalTime: BigOExpression)
  extends Implementation(method, Map(), internalTime, None)

object UniversalImplementations {
  implicit def stringToMethod(name: String): Method = Method(name)

  def universalImplementations = {
    val impl1 = UniversalImplementation("getByIndex", Map(Method("getNext") -> ConstantTime, Method("getFirst") -> LinearTime), ConstantTime)
    Set(impl1)
  }
}