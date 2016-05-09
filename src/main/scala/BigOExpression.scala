/**
  * Created by buck on 5/1/16.
  */
case class BigOExpression(powerOfN: Float, powerOfLogN: Int) extends Ordered[BigOExpression] {
  def compare(other: BigOExpression): Int = {
    val differenceInPowerOfN = this.powerOfN - other.powerOfN

    if (differenceInPowerOfN != 0)
      Math.signum(differenceInPowerOfN).toInt
    else
      this.powerOfLogN - other.powerOfLogN
  }
  def +(other: BigOExpression): BigOExpression = {
    if (this > other) this else other
  }
  def *(other: BigOExpression): BigOExpression = {
    BigOExpression(this.powerOfN + other.powerOfN, this.powerOfLogN + other.powerOfLogN)
  }
  def or(other: BigOExpression): BigOExpression = {
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

object ConstantTime extends BigOExpression(0, 0)
object LogTime extends BigOExpression(0, 1)
object LinearTime extends BigOExpression(1, 0)
