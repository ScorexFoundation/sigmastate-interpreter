package sigmastate.serialization

import sigmastate.Values._

class BooleanLeafsSerializerSpecification extends TableSerializationSpecification {

  override val objects = Table(
    ("object", "bytes"),
    (FalseLeaf, Array[Byte](13)),
    (TrueLeaf, Array[Byte](12))
  )

  tableRoundTripTest("Boolean Leafs: Serializers round trip")
  tablePredefinedBytesTest("Boolean Leafs: Deserialize from predefined bytes")
}
