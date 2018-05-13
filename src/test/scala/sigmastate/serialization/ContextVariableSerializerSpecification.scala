package sigmastate.serialization

import sigmastate.Values._

class ContextVariableSerializerSpecification extends SerializationSpecification {

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
    predefinedBytesTest(Array(1, 7, 10), TaggedBox(10))
  }
}
