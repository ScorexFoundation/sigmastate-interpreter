package sigmastate.serialization

import sigmastate.utxo.Height

class HeightSerializerSpec extends SerializationSpecification {

  property("Height: Serializer round trip") {
      roundTripTest(Height)
  }

}
