package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigma.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer

case class NumericCastSerializer(code: OpCode,
                                 cons: (Value[SNumericType], SNumericType) => Value[SNumericType])
  extends ValueSerializer[Transformer[SNumericType, SNumericType]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: Transformer[SNumericType, SNumericType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .putType(obj.tpe)

  override def parseBody(r: SigmaByteReader): Value[SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    cons(input, tpe)
  }
}
