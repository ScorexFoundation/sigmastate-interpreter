package sigmastate.serialization

import sigmastate.AND
import sigmastate.Values.BooleanConstant

class AndSerializerSpecification extends TableSerializationSpecification {

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true), boolConst(true)), Array[Byte](37, 35, 0, 2, 3, 12, 12)),
    (AND(boolConst(true), boolConst(false)), Array[Byte](37, 35, 0, 2, 3, 12, 13)),
    (AND(boolConst(false), boolConst(true)), Array[Byte](37, 35, 0, 2, 3, 13, 12)),
    (AND(boolConst(false), boolConst(false)), Array[Byte](37, 35, 0, 2, 3, 13, 13))
  )

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  tableRoundTripTest("And: Serializer round trip")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")
}
