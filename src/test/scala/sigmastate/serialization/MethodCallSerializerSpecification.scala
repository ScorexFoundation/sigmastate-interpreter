package sigmastate.serialization

import sigmastate.SNumericType
import sigmastate.Values.IntConstant
import sigmastate.lang.Terms.MethodCall

class MethodCallSerializerSpecification extends SerializationSpecification {

  property("MethodCall deserialization round trip") {
    val expr = MethodCall(IntConstant(10), SNumericType.getMethodByName("toByte"), IndexedSeq.empty)
    roundTripTest(expr)
  }

}
