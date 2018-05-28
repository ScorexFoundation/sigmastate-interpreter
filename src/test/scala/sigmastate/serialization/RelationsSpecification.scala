package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.ValueSerializer._
import OpCodes._

class RelationsSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (LT(IntConstant(2), IntConstant(3)), Array[Byte](LtCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 2, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 3)),
      (LE(IntConstant(2), IntConstant(3)), Array[Byte](LeCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 2, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 3)),
      (GT(IntConstant(6), IntConstant(5)), Array[Byte](GtCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 6, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 5)),
      (GE(IntConstant(6), IntConstant(5)), Array[Byte](GeCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 6, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 5)),
      (EQ(TrueLeaf, FalseLeaf), Array[Byte](EqCode, TrueCode, FalseCode)),
      (NEQ(TrueLeaf, FalseLeaf), Array[Byte](NeqCode, TrueCode, FalseCode))
    )

  tableRoundTripTest("Relations: serializer round trip")
  tablePredefinedBytesTest("Relations: deserialize from predefined bytes")

  property("Relations: serialization LT(bool, bool) must fail") {
    assertThrows[Exception] {
      deserialize(Array[Byte](LtCode, 12, 13))
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
