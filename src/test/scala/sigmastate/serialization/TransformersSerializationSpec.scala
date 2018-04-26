package sigmastate.serialization

import sigmastate.SInt
import sigmastate.utxo.{Exists, Fold, ForAll, MapCollection}

class TransformersSerializationSpec extends SerializationSpecification {

  property("MapCollection: Serializer round trip") {
    forAll { mc: MapCollection[SInt.type, SInt.type] =>
      roundTripTest(mc)
    }
  }

  property("Exists: Serializer round trip") {
    forAll { e: Exists[SInt.type] =>
      roundTripTest(e)
    }
  }

  property("ForAll: Serializer round trip") {
    forAll { e: ForAll[SInt.type] =>
      roundTripTest(e)
    }
  }

  property("Fold: Serializer round trip") {
    forAll { f: Fold[SInt.type] =>
      roundTripTest(f)
    }
  }

}
