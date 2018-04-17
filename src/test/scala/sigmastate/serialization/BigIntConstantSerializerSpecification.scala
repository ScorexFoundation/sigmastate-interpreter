package sigmastate.serialization

import java.math.BigInteger

import sigmastate.Values.{BigIntConstant, IntConstant}

class BigIntConstantSerializerSpecification extends SerializationSpecification {

  property("BigIntConstant: Serializer round trip") {
    forAll { i: IntConstant =>
      roundTripTest(i)
    }
  }

  property("BigIntConstant = 0, deserialize from predefined bytes") {

    val value = BigIntConstant(BigInteger.valueOf(0))

    val bytes = Array[Byte](15, 0, 0, 0, 1, 0)

    predefinedBytesTest(bytes, value)
  }

  property("BigIntConstant deserialize from predefined bytes") {

    val value = BigIntConstant(new BigInteger(Array[Byte](1,2,3,4,5)))

    val bytes = Array[Byte](15, 0, 1, 0, 1, 1, 2, 3, 4, 5)

    predefinedBytesTest(bytes, value)
  }
}

