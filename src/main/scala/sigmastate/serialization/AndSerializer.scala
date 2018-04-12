package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.Values.{ConcreteCollection, Value}
import sigmastate._

object AndSerializer extends ValueSerializer[AND] {

  import ValueSerializer._


  override val opCode = ValueSerializer.AndCode
  val typeCode: TypeCode = SBoolean.typeCode

  def isBoolean(v: Value[SType]): Boolean = v.opCode == TrueCode || v.opCode == FalseCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val (res, consumed) = deserialize(bytes, pos)
    assert(res.opCode == ConcreteCollectionCode)
    val col = res.asInstanceOf[ConcreteCollection[SType]]
    assert(col.value.forall(isBoolean))
    (new AND(col.asInstanceOf[ConcreteCollection[SBoolean.type]]), consumed)
  }

  override def serializeBody(and: AND): Array[TypeCode] = ValueSerializer.serialize(and.input)
}
