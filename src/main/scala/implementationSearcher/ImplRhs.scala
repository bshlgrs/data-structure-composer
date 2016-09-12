package implementationSearcher

import shared.{ConstantTime, BigOLiteral}

/**
  * Created by buck on 7/25/16.
  *
  */
case class ImplRhs(constant: BigOLiteral, costs: Map[MethodExpr, BigOLiteral] = Map()) {
  def +(other: ImplRhs): ImplRhs = {
    val combinedCosts = costs.foldLeft(other.costs) {
      case (currCosts: Map[MethodExpr, BigOLiteral], (methodExpr: MethodExpr, bigOLiteral: BigOLiteral)) => {
        currCosts + (methodExpr -> (currCosts.getOrElse(methodExpr, ConstantTime) + bigOLiteral))
      }
    }
    ImplRhs(this.constant + other.constant, combinedCosts)
  }

//  // sub in values
//  def bindToMap(settledImpls: Set[UnfreeImpl]): ImplRhs = {
//    costs.foldLeft(ImplRhs(this.constant))({case (implRhs: ImplRhs, (methodExpr: MethodExpr, lit: BigOLiteral)) =>
//      methodExpr.getFastestRelevantImplementationIfAny(settledImpls) match {
//        case None => implRhs + ImplRhs(ConstantTime, Map(methodExpr -> lit))
//        case Some(unfreeImpl) => implRhs + unfreeImpl.rhs
//      }
//    })
//  }
}

//case object ImplRhs {
//  val ConstantTime: ImplRhs = ImplRhs(ConstantTime)
//}
