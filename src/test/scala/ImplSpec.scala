/**
  * Created by buck on 9/12/16.
  */
import org.scalatest.FunSpec

class ImplSpec extends FunSpec {

  describe("Impl") {
    describe("when empty") {
      it("should have size 0") {
        assert(Set.empty.size == 0)
      }

      it("should produce NoSuchElementException when head is invoked") {
        assertThrows[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
