package simple_language

/**
  * Created by buck on 9/28/16.
  */

abstract class Statement {
  def run(scope: Scope): Scope
}

case class VariableDefinitionStatement(name: String, rhs: Expression)

case class LangError(name: String)

class Scope(vars: Map[String, Value], parentScope: Option[Scope]) {
  def get(name: String): Option[Value] = {
    vars.get(name) match {
      case Some(x) => Some(x)
      case None => parentScope.flatMap(_.get(name))
    }
  }
}


abstract class Expression {
  def eval(scope: Scope): Either[LangError, Value] = {
    ???
  }
}

case class FunctionCallExpression(args: List[Expression]) extends Expression
case class VariableExpression(name: String) extends Expression

case class MathOperator(symbol: String) extends Value
//case object AddOperator extends MathOperator("+")
//case object MultiplyOperator extends MathOperator("*")

abstract class Value {
}

case class IntValue(int: Int) extends Value


/*

stock_runs := (arr) -> {
  up := longest_increasing_seq(arr)
  down := longest_increasing_seq(arr.reversed())
  return List(up, down).max
}

longest_increasing_seq := (arr) -> {
  current := 1
  best := 0
  prev := arr.get(0)

  arr.forEach((x) -> {
    if (prev < x) {
      current += 1
      if (current > best) {
        best = current
      } else {
        current = 0;
      }
    }
  });

  return best;
}


 */
