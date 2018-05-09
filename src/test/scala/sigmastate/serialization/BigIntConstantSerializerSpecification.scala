package sigmastate.serialization

import java.math.BigInteger

import sigmastate.Values.BigIntConstant
import sigmastate.serialization.ValueSerializer.deserialize

class BigIntConstantSerializerSpecification extends TableSerializationSpecification {

  override def objects = Table(
    ("object", "bytes"),
    (BigIntConstant(BigInteger.valueOf(0)), Array[Byte](15, 0, 1, 0)),
    (BigIntConstant(new BigInteger(Array[Byte](3, 4, 5))), Array[Byte](15, 0, 3, 3, 4, 5))
  )


  tableRoundTripTest("BigIntConstant: Serializer round trip")
  tablePredefinedBytesTest("BigIntConstant: deserialize from predefined bytes")

  property("Deserialization from the arbitrary position should get a valid object") {

    val junk = Array[Byte](1, 2, 3, 4, 5)
    val source = Array[Byte](15, 0, 3, 3, 4, 5)
    val target = BigIntConstant(new BigInteger(Array[Byte](3, 4, 5)))

    deserialize(source) shouldBe target
    deserialize(source ++ junk) shouldBe target

    val (actual, consumed) = deserialize(junk ++ source, junk.length)

    actual shouldBe target
    consumed shouldBe source.length
  }

  property("Deserialization from the source should get 2 BigIntConstant") {

    val pos = 0
    val source = Array[Byte](15, 0, 3, 3, 4, 5)
    val target = BigIntConstant(new BigInteger(Array[Byte](3, 4, 5)))

    deserialize(source) shouldBe target
    deserialize(source ++ source) shouldBe target

    val (actualFirst, consumedFirst) = deserialize(source ++ source, pos)
    val (actualSecond, consumedSecond) = deserialize(source ++ source, consumedFirst)

    actualFirst shouldBe target
    actualSecond shouldBe target
    consumedFirst shouldBe source.length
    consumedSecond shouldBe source.length
  }
}