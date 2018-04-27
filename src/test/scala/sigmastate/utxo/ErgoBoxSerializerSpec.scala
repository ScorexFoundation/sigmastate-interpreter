package sigmastate.utxo

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import sigmastate.serialization.generators.ValueGeneratots

class ErgoBoxSerializerSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with SerializationRoundTripSpec
  with ValueGeneratots {

  implicit val ergoBoxSerializer = ErgoBox.serializer

  property("ErgoBox: Serializer round trip") {
    forAll { b: ErgoBox =>
      roundTripTest(b)
    }
  }
}
