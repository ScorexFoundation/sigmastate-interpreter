package sigmastate.serialization

import sigmastate.Values.ByteArrayConstant

class ByteArrayConstantSerializerSpecification extends TableSerializationSpecification {

  override def objects = Table(
    ("object", "bytes"),
    (ByteArrayConstant(Array[Byte]()), Array[Byte](16, 0, 0, 0, 0, 0)),
    (ByteArrayConstant(Array[Byte](1)), Array[Byte](16, 0, 0, 0, 1, 1)),
    (ByteArrayConstant(Array[Byte](1, 2, 3, 4, 5)), Array[Byte](16, 0, 1, 0, 1, 1, 2, 3, 4, 5))
  )

  tableRoundTripTest("ByteArrayConstant: Serializer round trip")
  tablePredefinedBytesTest("ByteArrayConstant: deserialize from predefined bytes")
}

