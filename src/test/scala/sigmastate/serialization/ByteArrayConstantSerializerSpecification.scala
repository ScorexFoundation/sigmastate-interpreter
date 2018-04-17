package sigmastate.serialization

import com.google.common.primitives.Ints
import sigmastate.Values.ByteArrayConstant

class ByteArrayConstantSerializerSpecification extends TableSerializationSpecification {

  override def objects = Table(
    ("object", "bytes"),
    (ByteArrayConstant(Array[Byte]()), Array[Byte](16, 0, 0, 0, 0)),
    (ByteArrayConstant(Array[Byte](1)), Array[Byte](16, 0, 0, 0, 1, 1)),
    (ByteArrayConstant(Array[Byte](1, 2, 3, 4, 5)), Array[Byte](16, 0, 0, 0, 5, 1, 2, 3, 4, 5))
  )

  tableRoundTripTest("ByteArrayConstant: Serializer round trip")
  tablePredefinedBytesTest("ByteArrayConstant: deserialize from predefined bytes")

  checkConsumed(Array[Byte](16, 0, 0, 0, 0))
  checkConsumed(Array[Byte](16, 0, 0, 0, 1, 1))
  checkConsumed(Array[Byte](16, 0, 0, 0, 5, 1, 2, 3, 4, 5))

  def checkConsumed(array: Array[Byte]) {
    val (_, consumed) = ValueSerializer.deserialize(array, 0)
    consumed should equal (array.length)
  }
}
