package sigmastate.serialization

import sigmastate.Values._
import sigmastate.SBox
import sigmastate.lang.DefaultSigmaBuilder.mkValUse

class ValUseSerializerSpecification extends SerializationSpecification {

  property("ValUse: ValUseInt serializer round trip") {
    forAll { ti: ValUseInt =>
      roundTripTest(ti, enrichedReader(ti))
    }
  }

  property("ValUse: ValUseBox serializer round trip") {
    forAll { tb: ValUseBox =>
      roundTripTest(tb, enrichedReader(tb))
    }
  }

  property("ValUse deserialize from predefined bytes") {
    val vu = mkValUse(10, SBox)
    predefinedBytesTest(
      v = vu,
      bytes = Array(ValUse.opCode, 10),
      getReader = enrichedReader(vu))
  }
}
