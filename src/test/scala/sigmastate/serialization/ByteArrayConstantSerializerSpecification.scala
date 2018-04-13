package sigmastate.serialization

import sigmastate.Values.ByteArrayConstant

class ByteArrayConstantSerializerSpecification extends SerializationSpecification {

  property("ByteArrayConstant: Serializer round trip") {
    forAll { arr: ByteArrayConstant =>
      roundTripTest(arr)
    }
  }

  property("ByteArrayConstant: Deserialize predefined bytes") {
    val value = ByteArrayConstant(Array[Byte](1, 3, 5, 9, 10, 100))
    val bytes = Array[Byte](15, 0, 6, 1, 3, 5, 9, 10, 100)
    predefinedBytesTest(bytes, value)
  }

}
