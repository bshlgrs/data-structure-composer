package parsers

import fastparse.WhitespaceApi
import implementationSearcher._
import shared._

import scala.util.{Success, Try}

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

  lazy val implLhs: P[(ImplLhs, ImplDeclaration)] = P(methodName ~ ("[" ~ methodName.rep(1, sep=",") ~ "]").? ~ ("if " ~ implConditions).?).map({
    case (funcName, mbParameters, mbConditions) => {
      val parameters = mbParameters.getOrElse(Nil).toList
      ImplLhs(funcName, mbConditions.getOrElse(ImplPredicateMap.empty)) -> ImplDeclaration(parameters)
    }
  })

  lazy val methodName: P[MethodName] = P(name.map((x: String) => MethodName(x)))

  lazy val nakedImplLhs: P[(ImplLhs, ImplDeclaration)] = P(implLhs ~ End)

  lazy val implCondition: P[(String, String)] = P(name ~ "." ~ name)
  lazy val implConditions: P[ImplPredicateMap] = P(implCondition.rep(1, sep=",")).map((x) => ImplPredicateMap.fromListOfTuples(x.toList))

  lazy val impl: P[(Impl, ImplDeclaration)] = P(implLhs ~ "<-" ~ implRhs ~ Index).map({ case (lhs, decl, rhs, index) =>
    Impl(lhs, rhs) -> decl })

  lazy val nakedImpl: P[(Impl, ImplDeclaration)] = P(impl ~ End)

//  lazy val unfreeImpl: P[UnfreeImpl] = P(implLhs ~ "<-" ~ affineBigONameCombo).map({ case (lhs, rhs) => UnfreeImpl(lhs, rhs) })
//  lazy val unfreeImplTuple: P[(ImplLhs, AffineBigOCombo[MethodName])] = P(implLhs ~ "<-" ~ affineBigONameCombo)

//  lazy val nakedUnfreeImpl: P[UnfreeImpl] = P(unfreeImpl ~ End)

  lazy val namedFunctionExpr: P[NamedFunctionExpr] = {
    P(name.map((x) => NamedFunctionExpr(MethodName(x))))
  }

  lazy val anonymousFunctionExpr: P[AnonymousFunctionExpr] = {
    P("_" ~ ("{" ~/ name.!.rep(sep=",") ~ "}").? ~ ("<-" ~ affineBigONameCombo).?).map({case ((mbConditions, mbAbonc)) =>
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

  lazy val impls: P[Set[(Impl, ImplDeclaration)]] = P("\n".rep() ~ implLine.rep(1, sep="\n".rep) ~ "\n".rep() ~ End).map(_.toSet.flatten)

  lazy val implLine: P[Option[(Impl, ImplDeclaration)]] = P(impl.map(Some(_)) | ("//" ~ CharsWhile(_ != '\n')).map((_) => None))

  lazy val dataStructure: P[(String, DataStructure)] = {
    ("ds" ~/ implLhs ~/ "{" ~/ "\n".? ~ (" ".rep() ~ impl).rep(sep=lineSep) ~ lineSep ~ "}").map({
      case (l: ImplLhs, d: ImplDeclaration, impls: Seq[(Impl, ImplDeclaration)]) =>
        l.name.name -> DataStructure(d.parameters, l.conditions, impls.toSet.map((x: (Impl, ImplDeclaration)) => x._1))
    })
  }

  lazy val nakedDataStructure: P[(String, DataStructure)] = dataStructure ~ End

  lazy val dataStructureFile: P[Set[(String, DataStructure)]] = {
    P("\n".rep() ~ dataStructure.rep(sep="\n".rep()) ~ "\n".rep() ~ End).map(_.toSet)
  }

  lazy val adt: P[AbstractDataType] = {
    ("adt" ~ name ~ "{" ~ "\n".? ~ (" ".rep() ~ methodExpr ~ "->" ~ bigOLiteral).rep(sep=lineSep) ~ lineSep ~ "}").map({
      case (l: String, adtImpls: Seq[(MethodExpr, BigOLiteral)]) =>
        new AbstractDataType(Map(), adtImpls.map({case ((mExpr, cost)) => mExpr -> cost}).toMap)
    })
  }

  lazy val lineSep: P[Unit] = P("\n" | ";")

  lazy val nakedAdt: P[AbstractDataType] = adt ~ End

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

  def parseImplFileString(stuff: String): Try[(Set[Impl], Map[MethodName, ImplDeclaration])] = {
    def blankImpl(name: MethodName): Impl = {
      Impl(ImplLhs(name, ImplPredicateMap.empty), AffineBigOCombo({ ??? }, { ??? }))
    }

    for {
      tuples: Set[(Impl, ImplDeclaration)] <- Try(impls.parse(stuff).get.value)
      fileImpls <- Success(tuples.map(_._1))

      // these are the methods which are never defined, but which are used in other definitions
      implicitDeclarationTuples <-
        Success(fileImpls.flatMap(_.rhs.keys.flatMap((x) =>
          if (x.args.isEmpty) Option(blankImpl(x.name) -> ImplDeclaration.empty) else Nil))
        )
      fileDecls <- Try {
        (tuples ++ implicitDeclarationTuples).groupBy(_._1.lhs.name).map({ case (m: MethodName, s: Set[(Impl, ImplDeclaration)]) => {
          assert(s.forall(_._2 == s.head._2))
          m -> s.head._2
        }})
      }
    } yield (fileImpls, fileDecls)
  }

  def parseDataStructureFileString(stuff: String, impls: Set[Impl], decls: Map[MethodName, ImplDeclaration]):
    Try[Map[String, DataStructure]] = {

    for {
      dataStructureSyntax <- Try(dataStructureFile.parse(stuff).get.value)
      res <- Try(dataStructureSyntax.groupBy(_._1).map({ case (name, setOfTuples) => {
        assert(setOfTuples.size == 1, s"there were ${setOfTuples.size} data structures named $name")
        setOfTuples.head
      }}))
    } yield res
  }
}
