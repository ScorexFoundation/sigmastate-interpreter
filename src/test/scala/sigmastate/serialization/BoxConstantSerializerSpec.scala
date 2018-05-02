package sigmastate.serialization

import sigmastate.Values.BoxConstant

class BoxConstantSerializerSpec extends SerializationSpecification {

  //todo: Fix when ergo box serialization will work
  ignore("BoxConstant: Serialization round trip") {
    forAll { bc: BoxConstant =>
      roundTripTest(bc)
    }
  }

}
