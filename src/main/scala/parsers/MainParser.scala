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

  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right
    }}
  }

  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
  val parens: P[Int] = P( "(" ~/ addSub ~ ")" )
  val factor: P[Int] = P( number | parens )

  val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
  val expr: P[Int]   = P( addSub ~ End )

  val StringChars = !"\"\\".contains(_: Char)

  val name: P[String] = P(CharIn('a'to'z').rep(1).!)

  val namedFunctionExpr: P[NamedFunctionExpr] = {
    P(CharIn('a'to'z').rep(1).!.map(NamedFunctionExpr))
  }

  val anonymousFunctionExpr: P[AnonymousFunctionExpr] = {
    P("_" ~ ("[" ~/ name.!.rep(1, sep=",") ~ "]").? ~ ("<-" ~ implRhs).?).map({case ((mbConditions, rhs)) =>
      AnonymousFunctionExpr(mbConditions.map(_.toSet).getOrElse(Set()), rhs.getOrElse(ImplRhs(ConstantTime)))
    })
  }

  val functionExpr: P[FunctionExpr] = namedFunctionExpr | anonymousFunctionExpr

  val bigOLiteral: P[BigOLiteral] = {
    "1".!.map((_) => ConstantTime) |
      "n".!.map((_) => LinearTime) |
      "log(n)".!.map((_) => LogTime)
  }

  val implRhs: P[ImplRhs] = bigOLiteral.map(ImplRhs(_, Map()))

  val methodExpr: P[MethodExpr] = P(name.! ~ ("[" ~ functionExpr.rep(1, sep=",") ~ "]").?).map({case ((x, mbFunctions)) =>
    MethodExpr(x, mbFunctions.map(_.toList).getOrElse(Nil))
  })

  def check(str: String, num: Int) = {
    val Parsed.Success(value, _) = expr.parse(str)
    assert(value == num)
  }

  def main (args: Array[String]) {
    println(implRhs.parse("1"))
    println(anonymousFunctionExpr.parse("_[hello,world] <- 1"))
    println(anonymousFunctionExpr.parse("_[hello,world] <- 1"))
//    println(bigOLiteral.parse("log(n)"))
    println(methodExpr.parse("f[x,y,_]"))
  }
}
