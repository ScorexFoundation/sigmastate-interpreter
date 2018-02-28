package sigmastate.serialization

import scorex.crypto.encode.Base58
import sigmastate._

class TaggedVariableSerializerSpecification extends SerializationSpecification {
  property("TaggedVariable serializer roundtrip") {
    roundTripTest(TaggedBox(21))
  }

  property("TaggedVariable deserialize from predefined bytes") {
    predefinedBytesTest(Base58.decode("M21").get, TaggedBox(10))
  }
}
