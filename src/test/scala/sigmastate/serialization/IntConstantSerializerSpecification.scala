package sigmastate.serialization

import scorex.crypto.encode.Base58
import sigmastate.Values.IntConstant

class IntConstantSerializerSpecification extends SerializationSpecification {

  property("IntConstant: Serializer round trip") {
    forAll { i: IntConstant =>
      roundTripTest(i)
    }
  }

  property("IntConstant deserialize from predefined bytes") {
    val value = IntConstant(1)
    val bytes = Base58.decode("981jCC8syJPa").get
    predefinedBytesTest(bytes, value)
  }
}
