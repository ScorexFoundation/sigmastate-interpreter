package sigmastate.serialization

import sigmastate.Values.BigIntConstant

class BigIntConstantSerializerSpecification extends SerializationSpecification {

  implicit def intToBigInt(i: Int): java.math.BigInteger = BigInt(i).bigInteger

  property("BigIntConstant: Serializer round trip") {
    forAll { i: BigIntConstant =>
      roundTripTest(i)
    }
  }

  property("BigIntConstant: deserialize from predefined bytes") {
    predefinedBytesTest(Array[Byte](16, 1, 1), BigIntConstant(1))
    predefinedBytesTest(Array[Byte](16, 1, 100), BigIntConstant(100))
    predefinedBytesTest(Array[Byte](16, 2, 0, -56), BigIntConstant(200))
    predefinedBytesTest(Array[Byte](16, 3, 15, 66, 64), BigIntConstant(1000000))
  }
}
