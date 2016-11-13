package tests

/**
  * Created by buck on 9/12/16.
  */

import finatraServer.{SearchController, SearchRequest}
import implementationSearcher._
import org.scalatest.FunSpec

class ServerSpec extends FunSpec {
  val controller = new SearchController
  describe("Search") {
    it("does not fail a regression test") {
      val result = controller.search(
        SearchRequest(None, None,
          Set("deleteMinimum!",
            "deleteLast!",
            "insertLast!",
            "getMinimum")))

      result.get
    }

    it("does not fail a different regression test") {
      val result = controller.search(
        SearchRequest(None, None,
          Set("deleteMinimum!",
            "deleteAtIndex!",
            "deleteMaximum!",
            "getMinimum",
            "getMaximum",
            "insertAnywhere!")))

      result.get
    }

    it("does not fail a third regression test, thanks Claire for spotting this one!") {
      val result = controller.search(
        SearchRequest(None, None,
          Set("insertLast!",
            "deleteLast!",
            "getLast",
            "insertBeforeFrontNode!",
            "reduce[_]",
            "updateNode!")))

      assert(result.get.items.size > 0)
    }


  }

}
