package tests

import implementationSearcher._
import org.scalatest.FunSpec
import shared._
import org.scalatest.prop.Checkers

class ImplLibrarySpecs extends FunSpec with Checkers {
  describe("arrows and closures") {
    it("works in a simple case") {
      val (impls, decls) = ImplDeclaration.parseMany("x <- y", "y <- z")
      val library = ImplLibrary(impls, decls, Map())

      def wrap(answer: Map[String, Set[String]]): Map[MethodName, Set[MethodName]] =
        answer.map({case (k: String, s: Set[String]) => MethodName(k) -> s.map((n) => MethodName(n)) })

      assert(library.backwardImplArrows == wrap(Map("y" -> Set("z"), "x" -> Set("y"))))
      assert(library.forwardImplArrows == wrap(Map("z" -> Set("y"), "y" -> Set("x"))))
      assert(library.closuresOfBackwardImplArrows ==
        wrap(Map("x" -> Set("x", "y", "z"), "y" -> Set("y", "z"), "z" -> Set("z"))))
      assert(library.closuresOfForwardImplArrows ==
        wrap(Map("z" -> Set("x", "y", "z"), "y" -> Set("y", "x"), "x" -> Set("x"))))
    }
  }
}
