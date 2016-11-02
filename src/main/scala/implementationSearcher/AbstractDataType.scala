package implementationSearcher

import parsers.MainParser
import shared.BigOLiteral

/**
  * Created by buck on 9/22/16.
  */

// to make it more generalized, we should also have a list of parameters, and methods should be a Set[MethodExpr]
case class AbstractDataType(parameters: Map[MethodName, BigOLiteral],
                            methods: Map[MethodExpr, BigOLiteral]) {

}

object AbstractDataType {
  def parse(string: String): AbstractDataType = {
    // todo
    MainParser.nakedAdt.parse(string).get.value
  }
}
