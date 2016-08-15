package secondNewTry

/**
  * Created by buck on 7/25/16.
  */

// Implements a function of the form a1 * b1 + a2 * b2 + a3 * b3 + bConstant
case class AffineFunction[A, B](constant: B, costs: Map[A, B])
