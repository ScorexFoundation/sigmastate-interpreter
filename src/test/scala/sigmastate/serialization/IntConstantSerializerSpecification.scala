package sigmastate.serialization

import sigmastate.Values.IntConstant

class IntConstantSerializerSpecification extends SerializationSpecification {

  property("IntConstant: Serializer round trip") {
    forAll { i: IntConstant =>
      roundTripTest(i)
    }
  }

  property("IntConstant deserialize from predefined bytes") {
    val value = IntConstant(1)
    val bytes = Array[Byte](11, 0, 0, 0, 0, 0, 0, 0, 1)
    predefinedBytesTest(bytes, value)
  }
}
