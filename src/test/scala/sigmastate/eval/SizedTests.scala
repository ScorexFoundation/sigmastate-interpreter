package sigmastate.eval

import scalan.BaseCtxTests
import Sized._
import org.ergoplatform.ErgoConstants.MaxTokens
import sigmastate.interpreter.CryptoConstants._
import special.collection.Size

class EvaluationTest extends BaseCtxTests {

  def check[T](size: Size[T], expectedSize: Int) = {
    size.dataSize shouldBe expectedSize
  }

  test("Data sizes specification") {
    check(SizeHash, hashLength)
    check(SizeCreationInfo, 4 + hashLength)
    check(SizeTokenId, hashLength)
    check(SizeToken, hashLength + 8)
    check(SizeTokens, SizeToken.dataSize.toInt * MaxTokens.value)
  }

}
