package sigmastate.serialization

import sigmastate.Values.BooleanConstant
import sigmastate.serialization.OpCodes._
import sigmastate.{OR, SBoolean}

class OrSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (OR(boolConst(true), boolConst(true)),  Array[Byte](OrCode, 2, SBoolean.typeCode, 1, SBoolean.typeCode, 1)),
    (OR(boolConst(true), boolConst(false)), Array[Byte](OrCode, 2, SBoolean.typeCode, 1, SBoolean.typeCode, 0)),
    (OR(boolConst(false),boolConst(true)),  Array[Byte](OrCode, 2, SBoolean.typeCode, 0, SBoolean.typeCode, 1)),
    (OR(boolConst(false),boolConst(false)), Array[Byte](OrCode, 2, SBoolean.typeCode, 0, SBoolean.typeCode, 0))
  )

  tableRoundTripTest("Or: Serializer round trip")
  tablePredefinedBytesTest("Or: deserialize from predefined bytes")
}
