package sigmastate.serialization.transformers

import sigmastate.Values.{Value, ValueCompanion}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.{BooleanTransformer, BooleanTransformerCompanion}
import sigmastate.{SCollection, SBoolean, SType, SFunc}

case class BooleanTransformerSerializer[T <: SType]
(opDesc: BooleanTransformerCompanion,
 f: (Value[SCollection[T]], Value[SFunc]) => Value[SBoolean.type]) extends ValueSerializer[BooleanTransformer[T]] {

  override def serialize(obj: BooleanTransformer[T], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, opDesc.argInfos(0))
      .putValue(obj.condition, opDesc.argInfos(1))

  override def parse(r: SigmaByteReader): Value[SBoolean.type] = {
    val input = r.getValue().asCollection[T]
    val condition = r.getValue().asFunc
    f(input, condition)
  }

}
