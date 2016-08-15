package firstNewTry

/**
  * Created by buck on 5/3/16.
  */

import org.parboiled2._
import shared.{LinearTime, LogTime, ConstantTime}

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.{Failure, Success}

class ImplementationParser(val input: ParserInput) extends Parser {
  def InputImplementationFile: Rule1[(List[Implementation], List[DataStructure])] = rule {
    zeroOrMore(Line) ~ EOI ~> ((lines: Seq[(List[Implementation], List[DataStructure])]) =>
      (lines.flatMap(_._1).toList, lines.flatMap(_._2).toList)
    )
  }


  def Line: Rule1[(List[Implementation], List[DataStructure])] = rule {
    (ImplementationStatementP ~> ((x: Implementation) => (List(x), Nil))) |
      ("//" ~ zeroOrMore(noneOf("\n")) ~ "\n" ~> (() => (Nil, Nil))) |
      zeroOrMore(" ") ~ "\n" ~> (() => (Nil, Nil)) |
      DataStructureExprP ~> ((x: DataStructure) => (Nil, List(x)))
  }

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def DataStructureExprP: Rule1[DataStructure] = rule {
    // todo: check case
    ImplementationStatementLHS ~ "{\n" ~ DataStructureExprBody ~ "}" ~> (
      (name: String, args: Option[Seq[String]], conditions: Option[Seq[ImplementationPredicate]], implementations: List[Implementation]) =>
        DataStructure(name, args.getOrElse(Nil).toList, conditions.getOrElse(Nil).toList, implementations))
  }

  def DataStructureExprBody: Rule1[List[Implementation]] = rule {
    zeroOrMore(Line) ~> ((x: Seq[(List[Implementation], List[DataStructure])]) => x.flatMap(_._1).toList)
  }

  def ImplementationStatementP: Rule1[Implementation] = rule {
    ImplementationStatementLHS ~ "<-" ~ ImplementationExpressionP ~ "\n" ~> (
      (name: String, args: Option[Seq[String]], conditions: Option[Seq[ImplementationPredicate]], expression: ImplementationExpr) =>
        Implementation(name, args.getOrElse(Nil).toList, conditions.getOrElse(Nil).toList, expression))
  }

  def ImplementationStatementLHS = rule {
    MethodNameString ~ optional("[" ~ (capture(VariableNameString) * ",") ~ "]") ~ optional(ImplementationStatementCondition)
  }

  def ImplementationStatementCondition: Rule1[Seq[ImplementationPredicate]] = rule {
    "if " ~ (ImplementationPredicateP * ",")
  }

  def ImplementationPredicateP: Rule1[ImplementationPredicate] = rule {
    MethodNameString ~ str(".") ~ MethodNameString ~> ((x: String, y: String) => ImplementationPredicate(x, MethodProperty(y)))
  }

  def ImplementationExpressionP: Rule1[ImplementationExpr] = rule {
    Sum ~ zeroOrMore(
      "|" ~ Sum ~> ((_: ImplementationExpr).minWith(_)))
  }

  def Sum: Rule1[ImplementationExpr] = rule {
    Term ~ zeroOrMore(
      "+" ~ Term ~> ((_: ImplementationExpr) + _))
  }

  def Term: Rule1[ImplementationExpr] = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~> ((_: ImplementationExpr) * _))
  }

  def Factor: Rule1[ImplementationExpr] = rule { LinearTimeP | LogTimeP | ConstantTimeP | MethodUseP | Parens }

  def MethodUseP: Rule1[ImplementationExpr] = rule {
    MethodNameString ~ optional("[" ~ (FunctionExpressionP * ",") ~ "]") ~> (
      (name: String, args: Option[Seq[FunctionExpr]]) =>
        MethodUse(name, args.getOrElse(Nil).toList): ImplementationExpr)
  }

  def Parens = rule { "(" ~ ImplementationExpressionP ~ ')' }

  def LinearTimeP: Rule1[ImplementationExpr] = rule { wspStr("n") ~> (() => Constant(LinearTime)) }
  def LogTimeP: Rule1[ImplementationExpr] = rule { wspStr("log(n)") ~> (() => Constant(LogTime)) }
  def ConstantTimeP: Rule1[ImplementationExpr] = rule { wspStr("1") ~> (() => Constant(ConstantTime)) }

  def FunctionExpressionP: Rule1[FunctionExpr] = rule {
    AnonymousFunctionExpressionP | MethodFunctionExpressionP | (wspStr("_") ~> (() => EmptyFunctionExpr))
  }

  def MethodFunctionExpressionP: Rule1[MethodFunctionExpr] = rule {
    MethodNameString ~ optional("[" ~ (FunctionExpressionP * ",") ~ "]") ~> ((x: String, y: Option[Seq[FunctionExpr]]) =>
      MethodFunctionExpr(x, y.getOrElse(Nil).toList))
  }

  def AnonymousFunctionExpressionP: Rule1[AnonymousFunctionExpr] = rule {
    "func" ~ optional("{" ~ (MethodNameString * ",") ~ "}") ~ "<-" ~ ImplementationExpressionP ~> (
      (args: Option[Seq[String]], lhs: ImplementationExpr) => AnonymousFunctionExpr(args.getOrElse(Nil).toList.map(MethodProperty(_)), lhs))
  }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
  def MethodNameString: Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha) ~ optional("!")) ~ zeroOrMore(' ') ~> ((x: String) => x.trim()) }
  def VariableNameString = rule { oneOrMore(CharPredicate.Alpha) }

  //////////////////////////////////////


}

object ParserTest {
  def main(args: Array[String]) {
    val text = Source.fromFile("./materials/implementations/DataStructures.txt").getLines.toList.mkString("\n")
//    val text = "VectorList {\n    getByIndex <- 1\n    insertAtFront! <- 1\n    insertAtEnd! <- 1\n    deleteAtIndex! <- n\n    deleteBetweenNodes! <- n\n}"

    new ImplementationParser(text).InputImplementationFile.run() match {
      case Success(x) => println(x)
      case Failure(x: ParseError) => println(new ImplementationParser(text).formatError(x))
      case Failure(x) => println(x)
    }
  }
}
