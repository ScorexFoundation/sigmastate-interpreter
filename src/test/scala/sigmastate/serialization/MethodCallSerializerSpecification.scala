package sigmastate.serialization

import org.ergoplatform.Outputs
import sigmastate.Values.{FuncValue, ValUse}
import sigmastate.lang.Terms.MethodCall
import sigmastate.utxo.ExtractScriptBytes
import sigmastate._

class MethodCallSerializerSpecification extends SerializationSpecification {

  property("MethodCall deserialization round trip") {
    val expr = MethodCall(Outputs,
      SCollection.FlatMapMethod.withConcreteTypes(Map(SCollection.tIV -> SBox, SCollection.tOV -> SByte)),
      Vector(FuncValue(1, SBox, ExtractScriptBytes(ValUse(1, SBox)))),
      Map()
    )
    roundTripTest(expr)
  }

  property("MethodCall deserialization round trip (non-generic method)") {
    val expr = MethodCall(Outputs,
      SMethod(SCollection, "size", SFunc(SCollection[SBox.type], SInt), 1),
      Vector(),
      Map()
    )
    roundTripTest(expr)
  }
}
