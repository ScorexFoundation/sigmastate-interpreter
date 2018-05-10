package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.ValueSerializer._

class RelationsSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (LT(IntConstant(2), IntConstant(3)), Array[Byte](31, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (LE(IntConstant(2), IntConstant(3)), Array[Byte](32, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (GT(IntConstant(6), IntConstant(5)), Array[Byte](33, 11, 0, 0, 0, 0, 0, 0, 0, 6, 11, 0, 0, 0, 0, 0, 0, 0, 5)),
      (GE(IntConstant(6), IntConstant(5)), Array[Byte](34, 11, 0, 0, 0, 0, 0, 0, 0, 6, 11, 0, 0, 0, 0, 0, 0, 0, 5)),
      (EQ(TrueLeaf, FalseLeaf), Array[Byte](35, 12, 13)),
      (NEQ(TrueLeaf, FalseLeaf), Array[Byte](36, 12, 13))
    )

  tableRoundTripTest("Relations: serializer round trip")
  tablePredefinedBytesTest("Relations: deserialize from predefined bytes")

  property("Relations: serialization LT(bool, bool) must fail") {
    assertThrows[Error] {
      deserialize(Array[Byte](31, 12, 13))
    }
  }

  property("IsMember: Serializer round trip") {
    forAll { im: IsMember =>
      roundTripTest(im)
    }
  }

  property("If: Serializer round trip") {
    forAll { i: If[SInt.type] =>
      roundTripTest(i)
    }
  }
}
