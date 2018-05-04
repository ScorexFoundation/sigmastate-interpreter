package sigmastate.serialization

import sigmastate.Values.BoxConstant

class BoxConstantSerializerSpec extends SerializationSpecification {

  property("BoxConstant: Serialization round trip") {
    forAll { bc: BoxConstant =>
      roundTripTest(bc)
    }
  }

}
