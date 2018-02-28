package sigmastate.serialization

import scorex.crypto.encode.Base58
import sigmastate.IntConstant

class IntConstantSerializerSpecification extends SerializationSpecification {
  property("IntConstant serializer roundtrip") {
    roundTripTest(IntConstant(1))
  }

  property("IntConstant deserialize from predefined bytes") {
    val value = IntConstant(1)
    val bytes = Base58.decode("981jCC8syJPa").get
    predefinedBytesTest(bytes, value)
  }
}
