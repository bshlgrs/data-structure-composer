package firstNewTry

/**
  * Created by buck on 5/1/16.
  */

case class BigOLiteral(powerOfN: Double, powerOfLogN: Int) extends Ordered[BigOLiteral] {
  def compare(other: BigOLiteral): Int = {
    val differenceInPowerOfN = this.powerOfN - other.powerOfN

    if (differenceInPowerOfN != 0)
      Math.signum(differenceInPowerOfN).toInt
    else
      this.powerOfLogN - other.powerOfLogN
  }
  def +(other: BigOLiteral): BigOLiteral = {
    if (this > other) this else other
  }
  def *(other: BigOLiteral): BigOLiteral = {
    BigOLiteral(this.powerOfN + other.powerOfN, this.powerOfLogN + other.powerOfLogN)
  }
  def or(other: BigOLiteral): BigOLiteral = {
    if (this > other) other else this
  }

  override def toString = s"BigO($toShortString)"

  def toShortString = (powerOfN, powerOfLogN) match {
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

object ConstantTime extends BigOLiteral(0, 0)
object LogTime extends BigOLiteral(0, 1)
object SqrtTime extends BigOLiteral(0.5, 0)
object LinearTime extends BigOLiteral(1, 0)
