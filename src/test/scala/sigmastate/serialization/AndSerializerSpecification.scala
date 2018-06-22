package sigmastate.serialization

import sigmastate._
import sigmastate.Values.{BooleanConstant, Constant, IntConstant}
import sigmastate.serialization.OpCodes._

class AndSerializerSpecification extends TableSerializationSpecification {

  def boolConst(b: Boolean): BooleanConstant = BooleanConstant.fromBoolean(b)

  override def objects = Table(
    ("object", "bytes"),
    (AND(boolConst(true), boolConst(false)),
      Array[Byte](AndCode, ConcreteCollectionBooleanConstantCode, 2, 1)),
    (AND(Constant[SCollection[SBoolean.type]](Array[Boolean](false, true), SCollection(SBoolean))),
      Array[Byte](AndCode, SBoolean.embedIn(SCollection.CollectionTypeCode), 2, 2)),
    (AND(boolConst(true), EQ(IntConstant(1), IntConstant(1))),
      Array[Byte](AndCode, ConcreteCollectionCode, 2, SBoolean.typeCode, // collection type
        SBoolean.typeCode, 1,
        EqCode, SInt.typeCode, 2, // 1 encoded as 2 via signed int ZigZag VLQ encoding
        SInt.typeCode, 2)),
  )

  tableRoundTripTest("And: Serializer round trip on predefined values")
  tablePredefinedBytesTest("And: deserialize from predefined bytes")

  property("And: Serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      roundTripTest(tree)
    }
  }

}
