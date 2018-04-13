package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate._
import sigmastate.serialization.ValueSerializer.Position

object AndSerializer extends ValueSerializer[AND] {

  override val opCode = ValueSerializer.AndCode
  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = ValueSerializer.deserialize(bytes, pos)


  override def serializeBody(and: AND): Array[TypeCode] = ValueSerializer.serialize(and.input)
}
