package sigmastate.serialization

import sigmastate.OR
import sigmastate.Values.BooleanConstant
import sigmastate.serialization.OpCodes._

class OrSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (OR(boolConst(true), boolConst(true)),  Array[Byte](OrCode, 2, TrueCode, TrueCode)),
    (OR(boolConst(true), boolConst(false)), Array[Byte](OrCode, 2, TrueCode, FalseCode)),
    (OR(boolConst(false),boolConst(true)),  Array[Byte](OrCode, 2, FalseCode, TrueCode)),
    (OR(boolConst(false),boolConst(false)), Array[Byte](OrCode, 2, FalseCode, FalseCode))
  )

  tableRoundTripTest("Or: Serializer round trip")
  tablePredefinedBytesTest("Or: deserialize from predefined bytes")
}
