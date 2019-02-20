package sigmastate.serialization

import org.ergoplatform.Outputs
import sigmastate.Values.{FuncValue, ValUse}
import sigmastate.lang.Terms.MethodCall
import sigmastate.utxo.ExtractScriptBytes
import sigmastate.{SBox, SByte, SCollection}

class MethodCallSerializerSpecification extends SerializationSpecification {

  property("MethodCall deserialization round trip") {
    val expr = MethodCall(Outputs,
      SCollection.FlatMapMethod,
      Vector(FuncValue(1, SBox, ExtractScriptBytes(ValUse(1, SBox)))),
      Map(SCollection.tIV -> SBox, SCollection.tOV -> SByte)
    )
    roundTripTest(expr)
  }

}
