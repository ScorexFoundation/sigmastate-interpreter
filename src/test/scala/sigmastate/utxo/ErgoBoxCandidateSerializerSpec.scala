package sigmastate.utxo

import org.scalatest.PropSpec
import org.scalatest.TryValues._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import sigmastate.serialization.generators.ValueGeneratots

class ErgoBoxCandidateSerializerSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with SerializationRoundTripSpec
  with ValueGeneratots {

  private val ergoBoxCandidateSerializer = ErgoBoxCandidate.serializer

  property("ErgoBoxCandidate: Serializer round trip") {
    forAll { b: ErgoBoxCandidate =>
      val bytes = ergoBoxCandidateSerializer.toBytes(b)
      val b1 = ergoBoxCandidateSerializer.parseBytes(bytes).success.value
      b1.value shouldBe b.value
      b1.proposition shouldBe b.proposition
      b1.additionalRegisters shouldBe b.additionalRegisters
    }
  }
}
