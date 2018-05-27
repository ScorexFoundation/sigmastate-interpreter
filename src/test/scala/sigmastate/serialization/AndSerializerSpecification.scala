package sigmastate.serialization

import sigmastate.{AND, SBoolean}
import sigmastate.Values.BooleanConstant
import OpCodes._

class AndSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true), boolConst(true)),  Array[Byte](AndCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, TrueCode, TrueCode)),
    (AND(boolConst(true), boolConst(false)), Array[Byte](AndCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, TrueCode, FalseCode)),
    (AND(boolConst(false),boolConst(true)),  Array[Byte](AndCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, FalseCode, TrueCode)),
    (AND(boolConst(false),boolConst(false)), Array[Byte](AndCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, FalseCode, FalseCode))
  )

  tableRoundTripTest("And: Serializer round trip")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")
}
