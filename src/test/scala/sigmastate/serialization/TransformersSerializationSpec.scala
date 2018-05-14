package sigmastate.serialization

import sigmastate._
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

  property("Slice: Serializer round trip") {
    forAll { f: Slice[SInt.type] =>
      roundTripTest(f)
    }
  }

  property("Append: Serializer round trip") {
    forAll { f: Append[SInt.type] =>
      roundTripTest(f)
    }
  }

  property("Where: Serializer round trip") {
    forAll { f: Where[SInt.type] =>
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

  property("IntToByteArray: Serializer round trip") {
    forAll { itba: IntToByteArray =>
      roundTripTest(itba)
    }
  }

  property("DeserializeContext: Serializer round trip") {
    forAll { itba: DeserializeContext[SBoolean.type ] =>
      roundTripTest(itba)
    }
  }

  property("DeserializeRegister: Serializer round trip") {
    forAll { itba: DeserializeRegister[SBoolean.type ] =>
      roundTripTest(itba)
    }
  }

  property("ByteArrayToBigInt: Serializer round trip") {
    forAll { batbi: ByteArrayToBigInt =>
      roundTripTest(batbi)
    }
  }

  property("CalcBlake2b256: Serializer round trip") {
    forAll { b256: CalcBlake2b256 =>
      roundTripTest(b256)
    }
  }

  property("CalcSha256: Serializer round trip") {
    forAll { s256: CalcSha256 =>
      roundTripTest(s256)
    }
  }

  property("ByIndex: Serializer round trip") {
    forAll { bi: ByIndex[SInt.type ] =>
      roundTripTest(bi)
    }
  }

}
