package firstNewTry

/**
  * Created by buck on 5/3/16.
  */

import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.{Failure, Success}

class ImplParser(val input: ParserInput) extends Parser {
  def InputImplementationFile: Rule1[(List[Impl], List[DataStructure])] = rule {
    zeroOrMore(Line) ~ EOI ~> ((lines: Seq[(List[Impl], List[DataStructure])]) =>
      (lines.flatMap(_._1).toList, lines.flatMap(_._2).toList)
    )
  }


  def Line: Rule1[(List[Impl], List[DataStructure])] = rule {
    (ImplementationStatementP ~> ((x: Impl) => (List(x), Nil))) |
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
      (name: String, args: Option[Seq[String]], conditions: Option[Seq[ImplPredicate]], implementations: List[Impl]) =>
        DataStructure(name, args.getOrElse(Nil).toList, conditions.getOrElse(Nil).toList, implementations))
  }

  def DataStructureExprBody: Rule1[List[Impl]] = rule {
    zeroOrMore(Line) ~> ((x: Seq[(List[Impl], List[DataStructure])]) => x.flatMap(_._1).toList)
  }

  def ImplementationStatementP: Rule1[Impl] = rule {
    ImplementationStatementLHS ~ "<-" ~ ImplementationExpressionP ~ "\n" ~> (
      (name: String, args: Option[Seq[String]], conditions: Option[Seq[ImplPredicate]], expression: ImplRhs) =>
        Impl(name, args.getOrElse(Nil).toList, conditions.getOrElse(Nil).toList, expression))
  }

  def ImplementationStatementLHS = rule {
    MethodNameString ~ optional("[" ~ (capture(VariableNameString) * ",") ~ "]") ~ optional(ImplementationStatementCondition)
  }

  def ImplementationStatementCondition: Rule1[Seq[ImplPredicate]] = rule {
    "if " ~ (ImplementationPredicateP * ",")
  }

  def ImplementationPredicateP: Rule1[ImplPredicate] = rule {
    MethodNameString ~ str(".") ~ MethodNameString ~> ((x: String, y: String) => ImplPredicate(x, MethodProperty(y)))
  }

  def ImplementationExpressionP: Rule1[ImplRhs] = rule {
    Sum // ~ zeroOrMore(
      //"|" ~ Sum ~> ((_: ImplRhs).minWith(_)))
  }

  def Sum: Rule1[ImplRhs] = rule {
    Term ~ zeroOrMore(
      "+" ~ Term ~> ((_: ImplRhs) + _))
  }

  def Term: Rule1[ImplRhs] = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~> ((_: ImplRhs) * _))
  }

  def Factor: Rule1[ImplRhs] = rule { LinearTimeP | LogTimeP | ConstantTimeP | SqrtTimeP | MethodUseP | Parens }

  def MethodUseP: Rule1[ImplRhs] = rule {
    MethodNameString ~ optional("[" ~ (FunctionExpressionP * ",") ~ "]") ~> (
      (name: String, args: Option[Seq[FunctionExpr]]) =>
        MethodUse(name, args.getOrElse(Nil).toList): ImplRhs)
  }

  def Parens = rule { "(" ~ ImplementationExpressionP ~ ')' }

  def LinearTimeP: Rule1[ImplRhs] = rule { wspStr("n") ~> (() => Constant(LinearTime)) }
  def LogTimeP: Rule1[ImplRhs] = rule { wspStr("log(n)") ~> (() => Constant(LogTime)) }
  def SqrtTimeP: Rule1[ImplRhs] = rule { wspStr("sqrt(n)") ~> (() => Constant(SqrtTime)) }
  def ConstantTimeP: Rule1[ImplRhs] = rule { wspStr("1") ~> (() => Constant(ConstantTime)) }

  def FunctionExpressionP: Rule1[FunctionExpr] = rule {
    AnonymousFunctionExpressionP | MethodFunctionExpressionP | (wspStr("_") ~> (() => EmptyFunctionExpr))
  }

  def MethodFunctionExpressionP: Rule1[MethodFunctionExpr] = rule {
    MethodNameString ~ optional("[" ~ (FunctionExpressionP * ",") ~ "]") ~> ((x: String, y: Option[Seq[FunctionExpr]]) =>
      MethodFunctionExpr(x, y.getOrElse(Nil).toList))
  }

  def AnonymousFunctionExpressionP: Rule1[AnonymousFunctionExpr] = rule {
    "func" ~ optional("{" ~ (MethodNameString * ",") ~ "}") ~ "<-" ~ ImplementationExpressionP ~> (
      (args: Option[Seq[String]], lhs: ImplRhs) => AnonymousFunctionExpr(args.getOrElse(Nil).toList.map(MethodProperty(_)), lhs))
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

    new ImplParser(text).InputImplementationFile.run() match {
      case Success(x) => println(x)
      case Failure(x: ParseError) => println(new ImplParser(text).formatError(x))
      case Failure(x) => println(x)
    }
  }
}
