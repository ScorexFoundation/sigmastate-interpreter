package sigmastate.serialization

import sigmastate.Values._
import OpCodes._
import sigmastate.SBox

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
    predefinedBytesTest(TaggedBox(10), Array(TaggedVariableCode, 10, SBox.typeCode))
  }
}
