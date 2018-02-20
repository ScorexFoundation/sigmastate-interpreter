package sigmastate.serialization

import sigmastate._

class BooleanLeafsSerializerSpecification extends TableSerializationSpecification {
  override val objects =
    Table(
      ("object", "bytes"),
      (FalseLeaf, Array[Byte](13)),
      (TrueLeaf, Array[Byte](12))
    )
  tableRoundTripTest("Boolean leafs serializers roundtrip")
  tablePredefinedBytesTest("Boolean leafs deserialize from predefined bytes")
}
