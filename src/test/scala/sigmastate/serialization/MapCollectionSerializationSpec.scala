package sigmastate.serialization

import sigmastate.SInt
import sigmastate.utxo.MapCollection

class MapCollectionSerializationSpec extends SerializationSpecification {

  property("MapCollection: Serializer round trip") {
    forAll { mc: MapCollection[SInt.type, SInt.type] =>
      roundTripTest(mc)
    }
  }

}
