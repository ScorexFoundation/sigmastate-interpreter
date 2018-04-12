package sigmastate.serialization

import scorex.crypto.encode.Base58
import sigmastate.Values._

class TaggedVariableSerializerSpecification extends SerializationSpecification {

  property("TaggedVariable: TaggedInt serializer round trip") {
    forAll { ti: TaggedInt =>
      roundTripTest(ti)
    }
  }

  property("TaggedVariable: TaggedBox serializer round trip") {
    forAll { tb: TaggedBox =>
      roundTripTest(tb)
    }
  }

  property("TaggedVariable deserialize from predefined bytes") {
    predefinedBytesTest(Base58.decode("M21").get, TaggedBox(10))
  }

}
