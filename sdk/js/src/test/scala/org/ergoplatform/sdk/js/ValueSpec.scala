package org.ergoplatform.sdk.js

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.generators.ObjectGenerators

import java.math.BigInteger

class ValueSpec extends AnyPropSpec with Matchers with ObjectGenerators with ScalaCheckPropertyChecks {

  property("BigInt toHex()") {
     val bigValue = BigInteger.valueOf(Long.MaxValue).multiply(BigInteger.valueOf(2))
     val v = Isos.isoValueToConstant.from(BigIntConstant(bigValue))
     v.toHex() shouldBe "060900fffffffffffffffe"
  }
}
