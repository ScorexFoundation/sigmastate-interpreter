package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.BooleanTransformer
import sigmastate.{SBoolean, SCollection, SType}
import sigmastate.utils.Extensions._

case class BooleanTransformerSerializer[T <: SType]
(code: OpCode,
 f: (Value[SCollection[T]], Byte, Value[SBoolean.type]) => Value[SBoolean.type]) extends ValueSerializer[BooleanTransformer[T]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: BooleanTransformer[T], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .put(obj.id)
      .putValue(obj.condition)

  override def parseBody(r: SigmaByteReader): Value[SBoolean.type] = {
    val input = r.getValue().asCollection[T]
    val idByte = r.getByte()
    val condition = r.getValue().asValue[SBoolean.type]
    f(input, idByte, condition)
  }

}
