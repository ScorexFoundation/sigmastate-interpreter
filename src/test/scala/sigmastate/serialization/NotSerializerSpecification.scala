package sigmastate.serialization

import sigmastate.Not
import sigmastate.Values.BooleanConstant


class NotSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (Not(boolConst(true)), Array[Byte](39, 12)),
    (Not(boolConst(false)), Array[Byte](39, 13))
  )

  tableRoundTripTest("Not: Serializer round trip")
  tablePredefinedBytesTest("Not: deserialize from predefined bytes")
}