package org.ergoplatform.sdk.js

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.Values.{BigIntConstant, Constant, GroupElementConstant}
import sigmastate.basics.CryptoConstants.dlogGroup
import sigmastate.crypto.CryptoFacade
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.ConstantSerializer
import sigmastate.serialization.generators.ObjectGenerators

import java.math.BigInteger

class ValueSpec extends AnyPropSpec with Matchers with ObjectGenerators with ScalaCheckPropertyChecks {

  def test[T <: SType](c: Constant[T], expectedHex: String) = {
    val v = Isos.isoValueToConstant.from(c)
    val S = ConstantSerializer(DeserializationSigmaBuilder)
    Base16.encode(S.toBytes(c)) shouldBe expectedHex
    v.toHex() shouldBe expectedHex
  }

  property("BigInt toHex()") {
    val bigValue = BigInteger.valueOf(Long.MaxValue).multiply(BigInteger.valueOf(2))
    test(BigIntConstant(bigValue), "060900fffffffffffffffe")
  }

  property("GroupElement toHex()") {
    val ge = CryptoFacade.exponentiatePoint(dlogGroup.generator, BigInteger.valueOf(2))
    test(GroupElementConstant(ge), "0702c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5")
  }
}
