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

  lazy val dataStructure: P[(ImplLhs, ImplDeclaration, Set[String], Set[(Impl, ImplDeclaration)])] = {
    ("ds" ~/ implLhs ~/ ("extends" ~/ name.rep(sep=",")).? ~/ "{" ~/ "\n".? ~ (" ".rep() ~ implLine).rep(sep=lineSep) ~ lineSep ~ "}").map({
      case (l: ImplLhs, d: ImplDeclaration, e: Option[Seq[String]], impls: Seq[Option[(Impl, ImplDeclaration)]]) =>
        (l, d, e.map(_.toSet).getOrElse(Set()), impls.flatten.toSet)
    })
  }


  lazy val nakedDataStructure: P[(ImplLhs, ImplDeclaration, Set[String], Set[(Impl, ImplDeclaration)])] = dataStructure ~ End

  lazy val dataStructureFile: P[Set[(ImplLhs, ImplDeclaration, Set[String], Set[(Impl, ImplDeclaration)])]] = {
    P("\n".rep() ~ dataStructure.rep(sep="\n".rep()) ~ "\n".rep() ~ End).map(_.toSet)
  }

  lazy val adt: P[AbstractDataType] = {
    ("adt" ~ name ~ "{" ~/ "\n".? ~ (" ".rep() ~ methodExpr ~ "->" ~ bigOLiteral).rep(sep=lineSep) ~ lineSep ~ "}").map({
      case (l: String, adtImpls: Seq[(MethodExpr, BigOLiteral)]) =>
        new AbstractDataType(Map(), adtImpls.map({case ((mExpr, cost)) => mExpr -> cost}).toMap)
    })
  }

  lazy val lineSep: P[Unit] = P("\n".rep | ";")

  lazy val nakedAdt: P[AbstractDataType] = adt ~ "\n".rep() ~ End

  def parseImplFileString(stuff: String): Try[(Set[FreeImpl], ImplLibrary.Decls)] = {
    def blankImplLhs(name: MethodName): ImplLhs = {
      ImplLhs(name, ImplPredicateMap.empty)
    }

    for {
      tuples: Set[(Impl, ImplDeclaration)] <- Try(impls.parse(stuff).get.value)
      fileImpls <- Success(tuples.map(_._1))

      // these are the methods which are never defined, but which are used in other definitions
      implicitDeclarationLhses <-
        Try(fileImpls.flatMap(_.rhs.keys.flatMap((x) =>
          // If the method is used, but was never defined, add it to the list of ImplDeclarations
          // with some dummy information
          tuples.find(_._1.name == x.name) match {
            case Some(tuple) => {
              assert(x.args.length == tuple._2.parameters.length,
              s"You used the method expression $x, which has a different number of arguments from the" +
                s" definition ${tuple._1}")

              None
            }
            case None => {
              Some(blankImplLhs(x.name) -> ImplDeclaration(x.args.zipWithIndex.map({ case (f, int) =>
                MethodName(s"arg$int")
              })))
            }
          }
        )))
      fileDecls <- Try {
        (tuples.map((x) => x._1.lhs -> x._2) ++ implicitDeclarationLhses).groupBy(_._1.name).map({
          case (m: MethodName, s: Set[(ImplLhs, ImplDeclaration)]) => {
            assert(s.forall(_._2 == s.head._2), s"There were inconsistent definitions for $m: " +
              s"$s")
            m -> s.head._2
        }})
      }
    } yield (fileImpls.map(FreeImpl(_, FreeImplSource(None))), fileDecls)
  }

  def parseDataStructureFileString(stuff: String, decls: ImplLibrary.Decls):
    Try[Map[String, DataStructure]] = {

    for {
      dataStructureSyntax <- Try(dataStructureFile.parse(stuff).get.value)
      res <- Try(dataStructureSyntax.groupBy(_._1).map({ case (name, setOfTuples) => {
        assert(setOfTuples.size == 1, s"there were ${setOfTuples.size} data structures named $name")

        val (ImplLhs(MethodName(dsName), dsConditions), ImplDeclaration(dsParameters), extensionOf, impls) = setOfTuples.head

        dsName -> DataStructure.build(dsName, dsParameters, extensionOf, dsConditions, impls, decls)
      }}))
    } yield res
  }

  def parseSingleDataStructureFileString(stuff: String, decls: ImplLibrary.Decls):
   Try[(String, DataStructure, String, String)] = {

    for {
      parseResult <- Try(dataStructure.parse(stuff))
      dataStructureSyntax <- Try(parseResult.get.value)

      (ds, dsName) <- Try {
        val (ImplLhs(MethodName(dsName), dsConditions), ImplDeclaration(dsParameters), extensionOf, impls) = dataStructureSyntax

        DataStructure.build(dsName, dsParameters, extensionOf, dsConditions, impls, decls) -> dsName
      }
      (startText, endText) <- Success(stuff.splitAt(parseResult.index))
    } yield (dsName, ds, startText, endText)
  }
}
