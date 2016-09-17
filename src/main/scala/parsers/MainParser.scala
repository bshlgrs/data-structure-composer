package parsers

import fastparse.WhitespaceApi
import implementationSearcher._
import shared._

/**
  * Created by buck on 9/12/16.
  */
object MainParser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  lazy val name: P[String] = P(!(bigOLiteral) ~ (CharIn('a'to'z','A'to'Z').rep(1) ~ "!".?).!)

  lazy val implLhs: P[ImplLhs] = P(name ~ ("[" ~ name.rep(1, sep=",") ~ "]").? ~ ("if " ~ implConditions).?).map({case (funcName, mbParameters, mbConditions) =>
    val parameters = mbParameters.map(_.toList).getOrElse(Nil)
    ImplLhs(funcName, parameters, mbConditions.map(_.toList(parameters)))
  })

  lazy val implCondition: P[(String, String)] = P(name ~ "." ~ name)
  lazy val implConditions: P[ImplPredicateMap] = P(implCondition.rep(1, sep=",")).map((x) => ImplPredicateMap.fromListOfTuples(x.toList))

  lazy val impl: P[Impl] = P(implLhs ~ "<-" ~ implRhs).map({ case (lhs, rhs) => Impl(lhs, rhs) })

  lazy val unfreeImpl: P[UnfreeImpl] = P(implLhs ~ "<-" ~ implRhs).map({ case (lhs, rhs) => UnfreeImpl(lhs, rhs) })

  lazy val namedFunctionExpr: P[NamedFunctionExpr] = {
    P(CharIn('a'to'z').rep(1).!.map(NamedFunctionExpr))
  }

  lazy val anonymousFunctionExpr: P[AnonymousFunctionExpr] = {
    P("_" ~ ("[" ~/ name.!.rep(1, sep=",") ~ "]").? ~ ("<-" ~ implRhs).?).map({case ((mbConditions, rhs)) =>
      AnonymousFunctionExpr(mbConditions.map(_.toSet).getOrElse(Set()), rhs.getOrElse(ImplRhs(ConstantTime)))
    })
  }

  lazy val functionExpr: P[FunctionExpr] = namedFunctionExpr | anonymousFunctionExpr

  lazy val bigOLiteral: P[BigOLiteral] = {
    "1".!.map((_) => ConstantTime) |
      "n".!.map((_) => LinearTime) |
      "log(n)".!.map((_) => LogTime)
  }

  lazy val bigOAsImplRhs: P[ImplRhs] = bigOLiteral.map(ImplRhs(_, Map()))

  lazy val factorInImplRhs: P[ImplRhs] = ((bigOLiteral ~ "*").? ~ methodExpr).map({ case ((mbBigO, mExpr)) =>
    ImplRhs(ConstantTime, Map(mExpr -> mbBigO.getOrElse(ConstantTime)))
  })

  lazy val implRhs: P[ImplRhs] = (factorInImplRhs | bigOAsImplRhs).rep(1, sep="+").map(_.reduce(_.+(_)))

  lazy val methodExpr: P[MethodExpr] = P(name.! ~ ("[" ~ functionExpr.rep(1, sep=",") ~ "]").?).map({case ((x, mbFunctions)) =>
    MethodExpr(x, mbFunctions.map(_.toList).getOrElse(Nil))
  })

  lazy val impls: P[List[Impl]] = P(impl.rep(1, sep="\n")).map(_.toList)

  def main (args: Array[String]) {
    println(bigOLiteral.parse("1"))
    println(implLhs.parse("m[f]"))
    println(implRhs.parse("log(n)"))
    println(anonymousFunctionExpr.parse("_[hello,world] <- n * hello + log(n)"))
    println(anonymousFunctionExpr.parse("_[hello,world] <- 1"))

    println(methodExpr.parse("f[x,y,_]"))
    println(impl.parse("insertAtEnd! <- getEnd + insertAfterNode!"))
  }
}
