package sigmastate.serialization

import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.Gen
import sigmastate.Values.AvlTreeConstant

class AvlTreeConstantSerializerSpec extends SerializationSpecification {

  property("AvlTreeConstant: Serializer round trip") {
    forAll { atc: AvlTreeConstant =>
      val bytes = ValueSerializer.serialize(atc)
      val d1 = ValueSerializer.deserialize(bytes).asInstanceOf[AvlTreeConstant]
      val randomInt = Gen.chooseNum(1, 20).sample.get
      val randomBytes = Gen.listOfN(randomInt, arbByte.arbitrary).sample.get.toArray
      val d2 = ValueSerializer.deserialize(randomBytes ++ bytes, randomInt)._1.asInstanceOf[AvlTreeConstant]

      atc.value.startingDigest.sameElements(d1.value.startingDigest) shouldBe true
      atc.value.startingDigest.sameElements(d2.value.startingDigest) shouldBe true

      atc.value.keyLength shouldBe d1.value.keyLength
      atc.value.keyLength shouldBe d2.value.keyLength

      atc.value.valueLengthOpt shouldBe d1.value.valueLengthOpt
      atc.value.valueLengthOpt shouldBe d2.value.valueLengthOpt

      atc.value.maxNumOperations shouldBe d1.value.maxNumOperations
      atc.value.maxNumOperations shouldBe d2.value.maxNumOperations

      atc.value.maxDeletes shouldBe d1.value.maxDeletes
      atc.value.maxDeletes shouldBe d2.value.maxDeletes
    }
  }

}
