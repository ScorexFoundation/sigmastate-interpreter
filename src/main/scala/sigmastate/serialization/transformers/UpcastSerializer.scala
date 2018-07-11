package sigmastate.serialization.transformers

import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}

object UpcastSerializer extends ValueSerializer[Upcast[SNumericType, SNumericType]] {
  override val opCode: OpCode = OpCodes.Upcast

  override def serializeBody(obj: Upcast[SNumericType, SNumericType], w: ByteWriterSigmaValues): Unit =
    w.putValue(obj.input)
      .putType(obj.tpe)

  override def parseBody(r: ByteReader): Upcast[SNumericType, SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    Upcast(input, tpe)
  }
}
