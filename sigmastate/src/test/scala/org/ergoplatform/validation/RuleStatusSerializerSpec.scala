package org.ergoplatform.validation

import org.scalatest.Assertion
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.{SigmaSerializer, SerializationSpecification}

class RuleStatusSerializerSpec extends SerializationSpecification with SigmaTestingCommons {

  private def roundtrip(status: RuleStatus): Assertion = {
    implicit val ser = RuleStatusSerializer
    roundTripTest(status)
    roundTripTestWithPos(status)
  }

  property("RuleStatusSerializer round trip") {
    forAll(statusGen, MinSuccessful(100)) { status =>
      roundtrip(status)
    }
  }

  property("RuleStatusSerializer parse unrecognized status") {
    val unknownCode = 100.toByte
    val someByte = 10.toByte
    val nextByte = 20.toByte
    val bytes = Array[Byte](1, unknownCode, someByte, nextByte)
    val r =  SigmaSerializer.startReader(bytes)
    val s = RuleStatusSerializer.parse(r)
    s shouldBe ReplacedRule(0)
    val b = r.getByte()
    b shouldBe nextByte
  }

}
