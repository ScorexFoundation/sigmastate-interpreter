package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer

case class NumericCastSerializer(code: OpCode,
                                 cons: (Value[SNumericType], SNumericType) => Value[SNumericType])
  extends ValueSerializer[Transformer[SNumericType, SNumericType]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: Transformer[SNumericType, SNumericType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "NumericCast")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue (obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "tpe")
    w.putType (obj.tpe)
    SerializeLog.logPrintf(false, true, false, "tpe")

    SerializeLog.logPrintf(false, true, false, "NumericCast")
  }

  override def parseBody(r: SigmaByteReader): Value[SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    cons(input, tpe)
  }
}
