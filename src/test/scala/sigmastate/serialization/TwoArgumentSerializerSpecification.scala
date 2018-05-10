package sigmastate.serialization

import sigmastate.Values.{BigIntConstant, ByteArrayConstant, GroupElementConstant, IntConstant}
import sigmastate._
import sigmastate.interpreter.GroupSettings

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  private lazy val randomPoint = GroupSettings.dlogGroup.createRandomGenerator()

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(IntConstant(2), IntConstant(3)), Array[Byte](41, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Plus(IntConstant(2), IntConstant(3)), Array[Byte](42, 11, 0, 0, 0, 0, 0, 0, 0, 2, 11, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Xor(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))), Array[Byte](43, 16, 0, 3, 1, 2, 3, 16, 0, 2, 3, 4)),
      (AppendBytes(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))), Array[Byte](68, 16, 0, 3, 1, 2, 3, 16, 0, 2, 3, 4))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")

  property("MultiplyGroup: Serializer round trip") {
    forAll { (x1: GroupElementConstant, x2: GroupElementConstant) =>
      roundTripTest(MultiplyGroup(x1, x2))
    }
  }

  property("Exponentiate: Serializer round trip") {
    forAll { (x1: GroupElementConstant, x2: BigIntConstant) =>
      roundTripTest(Exponentiate(x1, x2))
    }
  }
}
