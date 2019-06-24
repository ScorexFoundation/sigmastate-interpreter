package sigmastate.eval

import scalan.BaseCtxTests
import sigmastate.eval.Sized._
import special.collection.Size

class SizedTests extends BaseCtxTests {

  def check[T](size: Size[T], expectedSize: Int) = {
    size.dataSize.toInt shouldBe expectedSize
  }

  test("Data sizes expected values") {
    check(SizeHash, 32)
    check(SizeCreationInfo, 4 + 32)
    check(SizeTokenId, 32)
    check(SizeToken, 32 + 8)
    check(SizeTokensMax, (32 + 8) * 255)
    check(SizePropositionBytesMax, 4 * 1024)
  }

}
