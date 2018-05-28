package sigmastate.serialization

import sigmastate.{OR, SBoolean}
import sigmastate.Values.BooleanConstant
import OpCodes._

class OrSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (OR(boolConst(true), boolConst(true)),  Array[Byte](OrCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, TrueCode, TrueCode)),
    (OR(boolConst(true), boolConst(false)), Array[Byte](OrCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, TrueCode, FalseCode)),
    (OR(boolConst(false),boolConst(true)),  Array[Byte](OrCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, FalseCode, TrueCode)),
    (OR(boolConst(false),boolConst(false)), Array[Byte](OrCode, ConcreteCollectionCode, 0, 2, SBoolean.typeCode, FalseCode, FalseCode))
  )


  tableRoundTripTest("Or: Serializer round trip")
  tablePredefinedBytesTest("Or: deserialize from predefined bytes")
}
