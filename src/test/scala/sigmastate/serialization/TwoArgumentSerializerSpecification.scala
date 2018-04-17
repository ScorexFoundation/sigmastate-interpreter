package sigmastate.serialization

import java.math.BigInteger

import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import sigmastate.Values.{BigIntConstant, ByteArrayConstant, GroupElementConstant, IntConstant}
import sigmastate._

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(IntConstant(2), IntConstant(3)), Array[Byte](40, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Plus(IntConstant(2), IntConstant(3)), Array[Byte](41, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      //(Xor(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4, 5))), Array[Byte](23, 11, 0, 0, 0, 0, 0, 0, 0, 6, 11, 0, 0, 0, 0, 0, 0, 0, 5)),
      //(AppendBytes(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4, 5))), Array[Byte](24, 11, 0, 0, 0, 0, 0, 0, 0, 6, 11, 0, 0, 0, 0, 0, 0, 0, 5)),
      //(MultiplyGroup(GroupElementConstant(Curve25519Point), GroupElementConstant(Curve25519Point)), Array[Byte](25, 12, 13)),
      //(Exponentiate(GroupElementConstant(Curve25519Point), BigIntConstant(BigInteger.valueOf(2.intValue()))), Array[Byte](26, 12, 13))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")
}
