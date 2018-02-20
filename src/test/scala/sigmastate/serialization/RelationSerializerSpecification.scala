package sigmastate.serialization

import scorex.crypto.encode.Base58
import sigmastate._
import sigmastate.serialization.SigmaSerializer.deserialize

class RelationSerializerSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (LT(IntConstant(2), IntConstant(3)), Base58.decode("4rSL7c2zGG9UMVp7sCiUkULyV4").get),
      (LE(IntConstant(2), IntConstant(3)), Base58.decode("534BTm8S6piPbJymrgrtX8dpBL").get),
      (GT(IntConstant(6), IntConstant(5)), Base58.decode("5Dg2ovDswPHJqMBQqM9ia9UCXN").get),
      (GE(IntConstant(6), IntConstant(5)), Base58.decode("5QHtA5KKmwrE5AM4pqJ8Lom3De").get),
      (EQ(TrueLeaf, FalseLeaf), Base58.decode("9QxU").get),
      (NEQ(TrueLeaf, FalseLeaf), Base58.decode("9kSQ").get)
    )

  tableRoundTripTest("Relation serializers roundtrip")
  tablePredefinedBytesTest("Relations deserialize from predefined bytes")

  property("serialization LT(bool, bool) must fail") {
    assertThrows[Error] {
      deserialize(Array[Byte](21, 12, 13))
    }
  }
}
