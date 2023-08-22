package sigma.util

import scalan.BaseNestedTests

class StringUtilTests extends BaseNestedTests {
  import StringUtil._

  describe("StringUtil methods") {
    it("fileName") {
      fileName("dir", "a", "b") shouldBe "dir/a/b"
    }
  }
}
