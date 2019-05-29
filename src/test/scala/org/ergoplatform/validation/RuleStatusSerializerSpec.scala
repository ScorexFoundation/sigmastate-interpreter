package org.ergoplatform.validation

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import org.scalatest.Assertion
import sigmastate.serialization.{SigmaSerializer, SerializationSpecification}

class RuleStatusSerializerSpecification extends SerializationSpecification {

  private def roundtrip[S <: RuleStatus](status: S): Assertion = {
    val w = SigmaSerializer.startWriter()
    RuleStatusSerializer.serialize(status, w)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes, 0)
    val res = RuleStatusSerializer.parse(r)
    res shouldBe status
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = SigmaSerializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = RuleStatusSerializer.parse(r2)
    res2 shouldBe status
  }

  implicit val statusArb: Arbitrary[RuleStatus] = Arbitrary(statusGen)

  val byteArrayGen: Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(1, 10)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield bytes.toArray

  val statusGen: Gen[RuleStatus] = Gen.oneOf(
    Gen.oneOf(EnabledRule, DisabledRule),
    arbShort.arbitrary.filter(_ >= ValidationRules.FirstRuleId).map(id => ReplacedRule(id)),
    byteArrayGen.map(xs => ChangedRule(xs))
    )

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
