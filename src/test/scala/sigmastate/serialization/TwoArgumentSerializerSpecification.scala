package sigmastate.serialization

import sigmastate.Values.{ByteArrayConstant, IntConstant}
import sigmastate._

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  // TODO: Yet remains to be done

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(IntConstant(2), IntConstant(3)), Array[Byte](40, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Plus(IntConstant(2), IntConstant(3)), Array[Byte](41, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Xor(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))), Array[Byte](42, 16, 0, 0, 0, 3, 1, 2, 3, 16, 0, 0, 0, 2, 3, 4)),
      (AppendBytes(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))), Array[Byte](43, 16, 0, 0, 0, 3, 1, 2, 3, 16, 0, 0, 0, 2, 3, 4)),
      //(MultiplyGroup(GroupElementConstant(defaultPoint), GroupElementConstant(defaultPoint)), Array[Byte](25, 12, 13)),
      //(Exponentiate(GroupElementConstant(Curve25519Point), BigIntConstant(BigInteger.valueOf(2.intValue()))), Array[Byte](26, 12, 13))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")
}
