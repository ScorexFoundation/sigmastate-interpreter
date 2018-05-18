package sigmastate.serialization

import sigmastate.AND
import sigmastate.Values.BooleanConstant

class AndSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true),boolConst(true)), Array[Byte](38, 20, 0, 2, 3, 12, 12)),
    (AND(boolConst(true),boolConst(false)), Array[Byte](38, 20, 0, 2, 3, 12, 13)),
    (AND(boolConst(false),boolConst(true)), Array[Byte](38, 20, 0, 2, 3, 13, 12)),
    (AND(boolConst(false),boolConst(false)), Array[Byte](38, 20, 0, 2, 3, 13, 13))
  )

  tableRoundTripTest("And: Serializer round trip")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")
}
