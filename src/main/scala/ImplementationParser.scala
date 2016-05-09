/**
  * Created by buck on 5/3/16.
  */

import java.io.File

import org.parboiled2._

import scala.io.Source
import scala.util.{Failure, Success}
import scala.collection.immutable.Seq

class ImplementationParser(val input: ParserInput) extends Parser {
  def InputFile: Rule1[List[ImplementationStmt]] = rule {
    zeroOrMore(Line) ~ EOI ~> ((x: Seq[Option[ImplementationStmt]]) => x.flatten.toList)
  }

  def Line: Rule1[Option[ImplementationStmt]] = rule {
    (ImplementationStatementP ~> ((x: ImplementationStmt) => Some(x))) | ("//" ~ zeroOrMore(noneOf("\n")) ~ "\n" ~> (() => None)) | zeroOrMore(" ") ~ "\n" ~> (() => None)
  }

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def ImplementationStatementP: Rule1[ImplementationStmt] = rule {
    ImplementationStatementLHS ~ "<-" ~ ImplementationExpressionP ~ "\n" ~> (
      (name: String, args: Option[Seq[String]], conditions: Option[Seq[ImplementationPredicate]], expression: ImplementationExpr) =>
        ImplementationStmt(name, args.getOrElse(Nil).toList, conditions.getOrElse(Nil).toList, expression))
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
}

object ParserTest {
  def main(args: Array[String]) {
    val text = Source.fromFile("./materials/implementations/MainImplementations.txt").getLines.toList.mkString("\n")

    val parser = new ImplementationParser(text)

    parser.InputFile.run() match {
      case Success(x) => println(x)
      case Failure(x: ParseError) => println(parser.formatError(x))
      case Failure(x) => println(x)
    }
  }
}
