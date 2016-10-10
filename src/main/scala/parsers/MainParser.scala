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

  lazy val name: P[String] = P(!bigOLiteral ~ (CharIn('a'to'z','A'to'Z').rep(1) ~ "!".?).!)

  lazy val implLhs: P[ImplLhs] = P(name ~ ("[" ~ name.rep(1, sep=",") ~ "]").? ~ ("if " ~ implConditions).?).map({
    case (funcName, mbParameters, mbConditions) => {
      val parameters = mbParameters.map(_.toList).getOrElse(Nil)
      ImplLhs(funcName, parameters, mbConditions.map(_.toList(parameters)))
    }
  })

  lazy val nakedImplLhs: P[ImplLhs] = P(implLhs ~ End)

  lazy val implCondition: P[(String, String)] = P(name ~ "." ~ name)
  lazy val implConditions: P[ImplPredicateMap] = P(implCondition.rep(1, sep=",")).map((x) => ImplPredicateMap.fromListOfTuples(x.toList))

  lazy val impl: P[Impl] = P(implLhs ~ "<-" ~ implRhs ~ Index).map({ case (lhs, rhs, index) => Impl(lhs, rhs, Some(StringSource(s"from $index"))) })

  lazy val nakedImpl: P[Impl] = P(impl ~ End)

  lazy val unfreeImpl: P[UnfreeImpl] = P(implLhs ~ "<-" ~ affineBigONameCombo).map({ case (lhs, rhs) => UnfreeImpl(lhs, rhs) })
  lazy val nakedUnfreeImpl: P[UnfreeImpl] = P(unfreeImpl ~ End)

  lazy val namedFunctionExpr: P[NamedFunctionExpr] = {
    P(name.map(NamedFunctionExpr))
  }

  lazy val anonymousFunctionExpr: P[AnonymousFunctionExpr] = {
    P("_" ~ ("{" ~/ name.!.rep(1, sep=",") ~ "}").? ~ ("<-" ~ affineBigONameCombo).?).map({case ((mbConditions, mbAbonc)) =>
      AnonymousFunctionExpr(mbConditions.map(_.toSet).getOrElse(Set()), mbAbonc.getOrElse(AffineBigOCombo(ConstantTime, Map())))
    })
  }

  lazy val affineBigONameCombo: P[AffineBigOCombo[MethodName]] = (factorInAbonc | bigOInAbonc).rep(1, sep="+").map(_.reduce(_.+(_)))
  lazy val nakedAffineBigONameCombo: P[AffineBigOCombo[MethodName]] = P(affineBigONameCombo ~ End)

  lazy val factorInAbonc: P[AffineBigOCombo[MethodName]] = ((bigOLiteral ~ "*").? ~ name).map({ case ((mbBigO, mName)) =>
    AffineBigOCombo(ConstantTime, Map(MethodName(mName) -> mbBigO.getOrElse(ConstantTime)))
  })

  lazy val bigOInAbonc: P[AffineBigOCombo[MethodName]] = bigOLiteral.map((x) => AffineBigOCombo[MethodName](x, Map()))

  lazy val functionExpr: P[FunctionExpr] = namedFunctionExpr | anonymousFunctionExpr

  lazy val bigOLiteral: P[BigOLiteral] = {
    "1".!.map((_) => ConstantTime) |
      "nlogn".!.map((_) => PolyAndLogTime(1,1)) |
      "n".!.map((_) => LinearTime) |
      "log(n)".!.map((_) => LogTime)
  }

  lazy val bigOAsImplRhs: P[AffineBigOCombo[MethodExpr]] = bigOLiteral.map((x) => AffineBigOCombo(x, Map()))

  lazy val factorInImplRhs: P[AffineBigOCombo[MethodExpr]] = {
    ((bigOLiteral ~ "*").? ~ methodExpr).map({ case ((mbBigO, mExpr)) =>
      AffineBigOCombo(ConstantTime, Map(mExpr -> mbBigO.getOrElse(ConstantTime)))
    })
  }


  lazy val backwardsFactorInImplRhs: P[AffineBigOCombo[MethodExpr]] = {
    (methodExpr ~ "*" ~ bigOLiteral).map({ case ((mExpr, bigO)) =>
      AffineBigOCombo(ConstantTime, Map(mExpr -> bigO))
    })
  }

  lazy val implRhs: P[AffineBigOCombo[MethodExpr]] = {
    (factorInImplRhs | bigOAsImplRhs | backwardsFactorInImplRhs).rep(1, sep="+").map(_.reduce(_.+(_)))
  }

  lazy val methodExpr: P[MethodExpr] = P(name.! ~ ("[" ~ functionExpr.rep(1, sep=",") ~ "]").?).map({case ((x, mbFunctions)) =>
    MethodExpr(x, mbFunctions.map(_.toList).getOrElse(Nil))
  })

  lazy val justMethodExpr: P[MethodExpr] = methodExpr ~ End

  lazy val impls: P[Set[Impl]] = P("\n".rep() ~ implLine.rep(1, sep="\n".rep) ~ "\n".rep() ~ End).map(_.toSet.flatten)

  lazy val implLine: P[Option[Impl]] = P(impl.map(Some(_)) | ("//" ~ CharsWhile(_ != '\n')).map((_) => None))

  lazy val dataStructure: P[DataStructure] = {
    ("ds" ~ implLhs ~ "{" ~ "\n" ~ (" ".rep() ~ unfreeImpl).rep(sep="\n") ~ "\n" ~ "}").map({case (l: ImplLhs, impls: Seq[UnfreeImpl]) =>
      DataStructure(l, impls.toSet)
    })
  }

  lazy val nakedDataStructure: P[DataStructure] = dataStructure ~ End

  lazy val dataStructureFile: P[Set[DataStructure]] = {
    P("\n".rep() ~ dataStructure.rep(sep="\n".rep()) ~ "\n".rep() ~ End).map(_.toSet)
  }

  def main (args: Array[String]) {
//    println(bigOLiteral.parse("1"))
//    println(implLhs.parse("m[f]"))
//    println(implRhs.parse("log(n)"))
//    println(anonymousFunctionExpr.parse("_[hello,world] <- n * hello + log(n)"))
//    println(anonymousFunctionExpr.parse("_[hello,world] <- 1"))
//
    println(dataStructure.parse("Array {\ngetByIndex <- 1\n}\n").get)
//    println(impl.parse("each[f] <- getByIndex * n + n * f"))
//    println(P(implRhs ~ End).parse("getByIndex * n"))

  }
}
