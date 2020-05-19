package sigmastate.serialization

import sigmastate.Values._
import sigmastate.SBox
import sigmastate.lang.DefaultSigmaBuilder.mkValUse

class TaggedVariableSerializerSpecification extends SerializationSpecification {

  property("TaggedVariable: TaggedInt serializer round trip") {
    forAll { ti: TaggedInt =>
      roundTripTest(ti, enrichedReader(ti))
    }
  }

  property("TaggedVariable: TaggedBox serializer round trip") {
    forAll { tb: TaggedBox =>
      roundTripTest(tb, enrichedReader(tb))
    }
  }

  property("TaggedVariable deserialize from predefined bytes") {
    val vu = mkValUse(10, SBox)
    predefinedBytesTest(
      v = vu,
      bytes = Array(ValUse.opCode, 10),
      getReader = enrichedReader(vu))
  }
}
