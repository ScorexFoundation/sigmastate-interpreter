package sigmastate.crypto

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import scala.scalajs.js

class ImportedSpec extends AnyPropSpec with Matchers {
  val p1_x = js.BigInt("58696697963552658225209022604545906839720992139040109583461478932841590007980")
  val p1_y = js.BigInt("99221890655421281687240657051313379827030356718751695543666346070432252019955")

  property("CryptoContext") {
    val ctx = new CryptoContextJs
    ctx shouldNot be(null)
    ctx.getModulus() shouldNot be(null)
    ctx.getModulus() shouldBe js.BigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663")
    ctx.getOrder() shouldBe js.BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
    ctx.validatePoint(p1_x, p1_y) shouldNot be(null)
  }
}
