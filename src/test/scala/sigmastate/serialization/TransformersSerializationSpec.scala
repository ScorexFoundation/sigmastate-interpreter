package sigmastate.serialization

import sigmastate.SInt
import sigmastate.utxo._

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

  property("SizeOf: Serializer round trip") {
    forAll { s: SizeOf[SInt.type] =>
      roundTripTest(s)
    }
  }

  property("ExtractAmount: Serializer round trip") {
    forAll { e: ExtractAmount =>
      roundTripTest(e)
    }
  }

  property("ExtractScriptBytes: Serializer round trip") {
    forAll { e: ExtractScriptBytes =>
      roundTripTest(e)
    }
  }

  property("ExtractBytes: Serializer round trip") {
    forAll { e: ExtractBytes =>
      roundTripTest(e)
    }
  }

  property("ExtractBytesWithNoRef: Serializer round trip") {
    forAll { e: ExtractBytesWithNoRef =>
      roundTripTest(e)
    }
  }

  property("ExtractId: Serializer round trip") {
    forAll { e: ExtractId =>
      roundTripTest(e)
    }
  }

  property("ExtractRegisterAs: Serializer round trip") {
    forAll { e: ExtractRegisterAs[SInt.type ] =>
      roundTripTest(e)
    }
  }

}
