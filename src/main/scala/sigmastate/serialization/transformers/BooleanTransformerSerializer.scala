package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.BooleanTransformer
import sigmastate.{SBoolean, SCollection, SFunc, SType}

case class BooleanTransformerSerializer[T <: SType]
(code: OpCode,
 f: (Value[SCollection[T]], Value[SFunc]) => Value[SBoolean.type]) extends ValueSerializer[BooleanTransformer[T]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: BooleanTransformer[T], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.condition)

  override def parseBody(r: SigmaByteReader): Value[SBoolean.type] = {
    val input = r.getValue().asCollection[T]
    val condition = r.getValue().asFunc
    f(input, condition)
  }

}
