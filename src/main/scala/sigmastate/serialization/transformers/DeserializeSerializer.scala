package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.Deserialize
import sigmastate.{SByteArray, SType}

object DeserializeSerializer extends ValueSerializer[Deserialize[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeCode

  override def serializeBody(obj: Deserialize[SType]): Array[Byte] = {
    Array(obj.tpe.typeCode) ++ ValueSerializer.serialize(obj.input)
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Deserialize[SType], Consumed) = {
    val tpeByte = bytes(pos)
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    val (input, c1) = ValueSerializer.deserialize(bytes, pos + 1)
    (Deserialize(input.asInstanceOf[Value[SByteArray.type]])(tpe), c1 + 1)
  }

}
