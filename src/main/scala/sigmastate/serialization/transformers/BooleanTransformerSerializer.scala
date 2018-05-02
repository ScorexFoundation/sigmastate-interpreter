package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.ValueSerializer._
import sigmastate.utxo.BooleanTransformer
import sigmastate.{SBoolean, SCollection, SType}

case class BooleanTransformerSerializer[T <: SType, R <: BooleanTransformer[T]]
(code: OpCode,
 f: (Value[SCollection[T]], Byte, Value[SBoolean.type]) => R) extends ValueSerializer[R] {

  override val opCode: OpCode = code

  override def parseBody(bytes: Array[OpCode], pos: Position): (R, Consumed) = {
    val (input, c1) = ValueSerializer.deserialize(bytes, pos)
    val idByte = bytes(pos + c1)
    val (condition, c2) = ValueSerializer.deserialize(bytes, pos + c1 + 1)
    val i = input.asInstanceOf[Value[SCollection[T]]]
    val c = condition.asInstanceOf[Value[SBoolean.type]]
    f(i, idByte, c) -> (c1 + 1 + c2)
  }

  override def serializeBody(obj: R): Array[OpCode] = {
    val inputBytes = ValueSerializer.serialize(obj.input)
    val idByte = obj.id
    val conditionBytes = ValueSerializer.serialize(obj.condition)
    inputBytes ++ Array(idByte) ++ conditionBytes
  }
}
