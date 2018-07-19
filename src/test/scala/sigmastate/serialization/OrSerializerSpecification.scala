package sigmastate.serialization

import sigmastate.Values.{BooleanConstant, Constant, IntConstant}
import sigmastate.serialization.OpCodes._
import sigmastate._

class OrSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (OR(boolConst(true), boolConst(false)),
      Array[Byte](OrCode, ConcreteCollectionBooleanConstantCode, 2, 1)),
    (OR(Constant[SCollection[SBoolean.type]](Array[Boolean](false, true), SCollection(SBoolean))),
      Array[Byte](OrCode, SBoolean.embedIn(SCollectionType.CollectionTypeCode), 2, 2)),
    (OR(boolConst(true), EQ(IntConstant(1), IntConstant(1))),
      Array[Byte](OrCode, ConcreteCollectionCode, 2, SBoolean.typeCode, // collection type
        SBoolean.typeCode, 1,
        EqCode, SInt.typeCode, 2, // 1 encoded as 2 via signed int ZigZag VLQ encoding
        SInt.typeCode, 2)),
  )

  tableRoundTripTest("Or: Serializer round trip on predefined values")
  tablePredefinedBytesTest("Or: deserialize from predefined bytes")

  property("Or: Serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(OR.apply))) { tree =>
      roundTripTest(tree)
    }
  }
}
