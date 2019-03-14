package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.BooleanTransformer
import sigmastate.{SBoolean, SCollection, SFunc, SType}

case class BooleanTransformerSerializer[T <: SType]
(code: OpCode,
 f: (Value[SCollection[T]], Value[SFunc]) => Value[SBoolean.type]) extends ValueSerializer[BooleanTransformer[T]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: BooleanTransformer[T], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "BooleanTransformer")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "condition")
    w.putValue(obj.condition)
    SerializeLog.logPrintf(false, true, false, "condition")

    SerializeLog.logPrintf(false, true, false, "BooleanTransformer")
  }

  override def parseBody(r: SigmaByteReader): Value[SBoolean.type] = {
    val input = r.getValue().asCollection[T]
    val condition = r.getValue().asFunc
    f(input, condition)
  }

}
