package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer.{OpCode, Position}
import sigmastate.{OR, SBoolean, SCollection}

object OrSerializer extends ValueSerializer[OR] {
  override val opCode: OpCode = ValueSerializer.OrCode
  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val (body, consumed) = ValueSerializer.deserialize(bytes, pos)
    new OR(body.asInstanceOf[Value[SCollection[SBoolean.type]]]) -> consumed
  }

  override def serializeBody(or: OR): Array[Byte] = ValueSerializer.serialize(or.input)
}
