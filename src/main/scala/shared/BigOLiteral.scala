package shared

/**
  * Created by buck on 5/1/16.
  */
abstract class BigOLiteral extends Ordered[BigOLiteral] {
  def compare(other: BigOLiteral): Int = (this, other) match {
    case (x: PolyAndLogTime, y: PolyAndLogTime) => x.compareToPolyAndLogTime(y)
    case (ZeroTime, ZeroTime) => 0
    case (ZeroTime, y: PolyAndLogTime) => -1
    case (x: PolyAndLogTime, ZeroTime) => 1
  }

  def +(other: BigOLiteral): BigOLiteral = {
    if (this > other) this else other
  }

  def *(other: BigOLiteral): BigOLiteral = (this, other) match {
    case (x: PolyAndLogTime, y: PolyAndLogTime) => x.multiply(y)
    case _ => ZeroTime
  }

  def or(other: BigOLiteral): BigOLiteral = {
    if (this > other) other else this
  }

  override def toString = s"BigO($toShortString)"

  val toShortString: String
}

case class PolyAndLogTime(powerOfN: Float, powerOfLogN: Int) extends BigOLiteral {
  def compareToPolyAndLogTime(other: PolyAndLogTime): Int = {
    val differenceInPowerOfN = this.powerOfN - other.powerOfN

    if (differenceInPowerOfN != 0)
      Math.signum(differenceInPowerOfN).toInt
    else
      this.powerOfLogN - other.powerOfLogN
  }

  def multiply(other: PolyAndLogTime): BigOLiteral = {
    PolyAndLogTime(this.powerOfN + other.powerOfN, this.powerOfLogN + other.powerOfLogN)
  }

  lazy val toShortString = (powerOfN, powerOfLogN) match {
    case (0.0, 0) => "1"
    case (1.0, 0) => "n"
    case (0.5, 0) => "sqrt(n)"
    case (power, 0) => s"n**$power"
    case (0.0, 1) => "log(n)"
    case (1.0, 1) => "n log(n)"
    case (0.0, power) => s"log(n)**$power"
    case (m, x) => s"n**$powerOfN log(n)**$powerOfLogN"
  }
}

object ConstantTime extends PolyAndLogTime(0, 0)
object LogTime extends PolyAndLogTime(0, 1)
object LinearTime extends PolyAndLogTime(1, 0)

object ZeroTime extends BigOLiteral {
  val toShortString = "0"
}
