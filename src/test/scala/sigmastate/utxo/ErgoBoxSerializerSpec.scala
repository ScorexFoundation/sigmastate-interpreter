package sigmastate.utxo

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.ergoplatform.ErgoBox
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.TryValues._
import sigmastate.serialization.generators.ValueGenerators

class ErgoBoxSerializerSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with SerializationRoundTripSpec
  with ValueGenerators {

  implicit val ergoBoxSerializer = ErgoBox.serializer

  property("ErgoBox: Serializer round trip") {
    forAll { b: ErgoBox =>
      val bytes = ergoBoxSerializer.toBytes(b)
      val b1 = ergoBoxSerializer.parseBytes(bytes).success.value

      b1.value shouldBe b.value
      b1.proposition shouldBe b.proposition
      b1.transactionId.sameElements(b.transactionId) shouldBe true
      b1.boxId shouldBe b.boxId
      b1.additionalRegisters shouldBe b.additionalRegisters
    }
  }

  property("ErgoBox: start pos and consumed bytes") {
    forAll { b: ErgoBox =>
      val randomBytesCount = Gen.chooseNum(1, 20).sample.get
      val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
      val bytes = ergoBoxSerializer.toBytes(b)
      ergoBoxSerializer.parseBody(randomBytes ++ bytes, randomBytesCount) shouldEqual (b, bytes.length)
    }
  }
}
