package sigmastate.serialization

import sigmastate.{AND, SBoolean}
import sigmastate.Values.BooleanConstant
import sigmastate.serialization.OpCodes._

class AndSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true), boolConst(true)),  Array[Byte](AndCode, 2, SBoolean.typeCode, 1, SBoolean.typeCode, 1)),
    (AND(boolConst(true), boolConst(false)), Array[Byte](AndCode, 2, SBoolean.typeCode, 1, SBoolean.typeCode, 0)),
    (AND(boolConst(false),boolConst(true)),  Array[Byte](AndCode, 2, SBoolean.typeCode, 0, SBoolean.typeCode, 1)),
    (AND(boolConst(false),boolConst(false)), Array[Byte](AndCode, 2, SBoolean.typeCode, 0, SBoolean.typeCode, 0))
  )

  tableRoundTripTest("And: Serializer round trip")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")
}
