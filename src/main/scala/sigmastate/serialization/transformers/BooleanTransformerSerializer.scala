package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.BooleanTransformer
import sigmastate.{SBoolean, SCollection, SType}

case class BooleanTransformerSerializer[T <: SType, R <: BooleanTransformer[T]]
(code: OpCode, f: (Value[SCollection[T]], Byte, Value[SBoolean.type]) => R) extends ValueSerializer[R] {

  override val opCode: OpCode = code

  override def serializeBody(obj: R, w: ByteWriter): Unit =
    w.putValue(obj.input)
      .put(obj.id)
      .putValue(obj.condition)

  override def parseBody(r: ByteReader): R = {
    val input = r.getValue().asCollection[T]
    val idByte = r.getByte()
    val condition = r.getValue().asValue[SBoolean.type]
    f(input, idByte, condition)
  }

}
