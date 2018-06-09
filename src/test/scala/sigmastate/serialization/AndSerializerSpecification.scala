package sigmastate.serialization

import sigmastate.AND
import sigmastate.Values.BooleanConstant
import sigmastate.serialization.OpCodes._

class AndSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true), boolConst(true)),  Array[Byte](AndCode, 0, 2, TrueCode, TrueCode)),
    (AND(boolConst(true), boolConst(false)), Array[Byte](AndCode, 0, 2, TrueCode, FalseCode)),
    (AND(boolConst(false),boolConst(true)),  Array[Byte](AndCode, 0, 2, FalseCode, TrueCode)),
    (AND(boolConst(false),boolConst(false)), Array[Byte](AndCode, 0, 2, FalseCode, FalseCode))
  )

  tableRoundTripTest("And: Serializer round trip")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")
}
