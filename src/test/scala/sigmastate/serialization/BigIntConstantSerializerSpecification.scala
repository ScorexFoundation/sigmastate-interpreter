package sigmastate.serialization

import java.math.BigInteger

import sigmastate.Values.{BigIntConstant, ByteArrayConstant, IntConstant}

class BigIntConstantSerializerSpecification extends TableSerializationSpecification {

  override def objects = Table(
    ("object", "bytes"),
    (BigIntConstant(BigInteger.valueOf(0)), Array[Byte](15, 0, 0, 0, 1, 0)),
    (BigIntConstant(new BigInteger(Array[Byte](1,2,3,4,5))), Array[Byte](15, 0, 1, 0, 1, 1, 2, 3, 4, 5))
  )

  tableRoundTripTest("BigIntConstant: Serializer round trip")
  tablePredefinedBytesTest("BigIntConstant: deserialize from predefined bytes")
}