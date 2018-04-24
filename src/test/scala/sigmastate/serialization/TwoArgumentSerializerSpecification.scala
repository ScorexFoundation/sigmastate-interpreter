package sigmastate.serialization

import java.math.BigInteger

import sigmastate.Values.{BigIntConstant, ByteArrayConstant, GroupElementConstant, IntConstant}
import sigmastate._
import sigmastate.interpreter.GroupSettings

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  private lazy val randomPoint = GroupSettings.dlogGroup.createRandomGenerator()

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(IntConstant(2), IntConstant(3)), Array[Byte](40, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Plus(IntConstant(2), IntConstant(3)), Array[Byte](41, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Xor(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))), Array[Byte](42, 16, 0, 3, 1, 2, 3, 16, 0, 2, 3, 4)),
      (AppendBytes(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))), Array[Byte](43, 16, 0, 3, 1, 2, 3, 16, 0, 2, 3, 4)),

      // TODO: uncomment and fix when a group will be considered
      //(MultiplyGroup(GroupElementConstant(randomPoint), GroupElementConstant(randomPoint)), Array[Byte](25, 12, 13)),
      //(Exponentiate(GroupElementConstant(randomPoint), BigIntConstant(BigInteger.valueOf(2.intValue()))), Array[Byte](26, 12, 13))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")
}
