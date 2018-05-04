package sigmastate.utxo

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.TryValues._
import sigmastate.serialization.generators.ValueGeneratots

class ErgoBoxSerializerSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with SerializationRoundTripSpec
  with ValueGeneratots {

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
}
