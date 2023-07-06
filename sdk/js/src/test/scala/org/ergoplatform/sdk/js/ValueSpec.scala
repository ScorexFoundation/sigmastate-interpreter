package org.ergoplatform.sdk.js

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.Values.{BigIntConstant, Constant, GroupElementConstant, SigmaPropConstant}
import sigmastate.basics.CryptoConstants.dlogGroup
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.crypto.CryptoFacade
import sigmastate.eval.{CSigmaProp, SigmaDsl}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.ConstantSerializer
import sigmastate.utils.Helpers
import special.sigma.SigmaTestingData

import java.math.BigInteger

class ValueSpec extends AnyPropSpec with Matchers with SigmaTestingData with ScalaCheckPropertyChecks {

  def test[T <: SType](c: Constant[T], expectedHex: String) = {
    val v = Isos.isoValueToConstant.from(c)
    val S = ConstantSerializer(DeserializationSigmaBuilder)
    Base16.encode(S.toBytes(c)) shouldBe expectedHex
    v.toHex() shouldBe expectedHex
    Isos.isoValueToConstant.to(Value.fromHex(expectedHex)) shouldBe c
  }

  property("BigInt toHex()/fromHex()") {
    val bigValue = BigInteger.valueOf(Long.MaxValue).multiply(BigInteger.valueOf(2))
    test(BigIntConstant(bigValue), "060900fffffffffffffffe")
  }

  property("GroupElement toHex()/fromHex()") {
    val ge = CryptoFacade.exponentiatePoint(dlogGroup.generator, BigInteger.valueOf(2))
    test(GroupElementConstant(ge), "0702c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5")
  }

  property("SigmaProp toHex()/fromHex()") {
    val c = SigmaPropConstant(
      CSigmaProp(
        ProveDlog(
          Helpers.decodeECPoint("0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e")
        )))
    test(c, "08cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e")
  }
}
