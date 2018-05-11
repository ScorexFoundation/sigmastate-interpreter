package sigmastate.serialization

import sigmastate.Values.CollectionConstant

class CollectionConstantSerializerSpecification extends TableSerializationSpecification {
  
  property("ByteArrayConstant: Serializer round trip") {
    forAll { arr: CollectionConstant =>
      roundTripTest(arr)
    }
  }

  property("ByteArrayConstant: Deserialize predefined bytes") {
    val value = CollectionConstant(Array[Byte](1, 3, 5, 9, 10, 100))
    val bytes = Array[Byte](16, 0, 6, 1, 3, 5, 9, 10, 100)
    predefinedBytesTest(bytes, value)
  }

  override def objects = Table(
    ("object", "bytes"),
    (CollectionConstant(Array[Byte]()), Array[Byte](16, 0, 0)),
    (CollectionConstant(Array[Byte](1)), Array[Byte](16, 0, 1, 1)),
    (CollectionConstant(Array[Byte](1, 2, 3, 4, 5)), Array[Byte](16, 0, 5, 1, 2, 3, 4, 5))
  )

  tableRoundTripTest("ByteArrayConstant: Serializer table round trip")
  tablePredefinedBytesTest("ByteArrayConstant: deserialize from predefined bytes")

  checkConsumed(Array[Byte](16, 0, 0))
  checkConsumed(Array[Byte](16, 0, 1, 1))
  checkConsumed(Array[Byte](16, 0, 5, 1, 2, 3, 4, 5))

  def checkConsumed(array: Array[Byte]) {
    val (_, consumed) = ValueSerializer.deserialize(array, 0)
    consumed should equal (array.length)
  }
}
