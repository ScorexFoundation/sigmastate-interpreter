package sigmastate.serialization

import sigmastate.Values.{ByteArrayConstant, GroupElementConstant, LongConstant, BigIntConstant}
import sigmastate._
import sigmastate.Values._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.Append
import OpCodes._

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(LongConstant(2), LongConstant(3)), Array[Byte](MinusCode, SLong.typeCode, 4, SLong.typeCode, 6)),
      (Plus(LongConstant(2), LongConstant(3)), Array[Byte](PlusCode, SLong.typeCode, 4, SLong.typeCode, 6)),
      (Multiply(LongConstant(2), LongConstant(3)), Array[Byte](MultiplyCode, SLong.typeCode, 4, SLong.typeCode, 6)),
      (Xor(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))),
          Array[Byte](XorCode, ByteArrayTypeCode, 0, 3, 1, 2, 3, ByteArrayTypeCode, 0, 2, 3, 4)),
      (Append(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))),
          Array[Byte](AppendCode, ByteArrayTypeCode, 0, 3, 1, 2, 3, ByteArrayTypeCode, 0, 2, 3, 4))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")

  property("ArithmeticOperations: Serializer round trip") {
    forAll(longConstGen, longConstGen, aritmeticCodeGen) { (x1: LongConstant, x2: LongConstant, opCode: OpCode) =>
      roundTripTest(ArithOp(x1, x2, opCode))
    }
  }

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
