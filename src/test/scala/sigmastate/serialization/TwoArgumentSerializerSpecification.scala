package sigmastate.serialization

import sigmastate.Values.{ByteArrayConstant, GroupElementConstant, IntConstant, BigIntConstant}
import sigmastate._
import sigmastate.Values._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.Append
import OpCodes._

class TwoArgumentSerializerSpecification extends TableSerializationSpecification {

  override val objects =
    Table(
      ("object", "bytes"),
      (Minus(IntConstant(2), IntConstant(3)), Array[Byte](MinusCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 2, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Plus(IntConstant(2), IntConstant(3)), Array[Byte](PlusCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 2, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Multiply(IntConstant(2), IntConstant(3)), Array[Byte](MultiplyCode, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 2, SInt.typeCode, 0, 0, 0, 0, 0, 0, 0, 3)),
      (Xor(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))),
          Array[Byte](XorCode, ByteArrayTypeCode, 0, 3, 1, 2, 3, ByteArrayTypeCode, 0, 2, 3, 4)),
      (Append(ByteArrayConstant(Array(1, 2, 3)), ByteArrayConstant(Array(3, 4))),
          Array[Byte](AppendCode, ByteArrayTypeCode, 0, 3, 1, 2, 3, ByteArrayTypeCode, 0, 2, 3, 4))
    )

  tableRoundTripTest("TwoArguments: serializer round trip")
  tablePredefinedBytesTest("TwoArguments: deserialize from predefined bytes")

  property("ArithmeticOperations: Serializer round trip") {
    forAll(intConstGen, intConstGen, aritmeticCodeGen) { (x1: IntConstant, x2: IntConstant, opCode: OpCode) =>
      roundTripTest(ArithmeticOperations(x1, x2, opCode))
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
