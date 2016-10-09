package implementationSearcher

import parsers.MainParser

/**
  * Created by buck on 9/22/16.
  */

// to make it more generalized, we should also have a list of parameters, and methods should be a Set[MethodExpr]
case class AbstractDataType(methods: Set[MethodName]) {

}

object AbstractDataType {
  def apply(string: String) = {
    MainParser
  }
}
