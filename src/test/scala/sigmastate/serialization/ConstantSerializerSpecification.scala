package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform._
import org.scalacheck.Arbitrary._
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{FalseLeaf, Constant, SValue, TrueLeaf, BigIntConstant, GroupGenerator, ByteArrayConstant, LongConstant}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate._
import sigmastate.Values._

class ConstantSerializerSpecification extends TableSerializationSpecification {

  private def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { x: Array[T#WrappedType] => roundTripTest(Constant[SCollection[T]](x, SCollection(tpe))) }
  }

  property("Constant serialization round trip") {
    forAll { x: Byte => roundTripTest(Constant[SByte.type](x, SByte)) }
    forAll { x: Boolean => roundTripTest(BooleanConstant.fromBoolean(x)) }
    forAll { x: Long => roundTripTest(Constant[SLong.type](x, SLong)) }
    forAll { x: BigInteger => roundTripTest(Constant[SBigInt.type](x, SBigInt)) }
    forAll { x: EcPointType => roundTripTest(Constant[SGroupElement.type](x, SGroupElement)) }
    forAll { x: ErgoBox => roundTripTest(Constant[SBox.type](x, SBox)) }
    forAll { x: AvlTreeData => roundTripTest(Constant[SAvlTree.type](x, SAvlTree)) }
    forAll { x: Array[Byte] => roundTripTest(Constant[SByteArray](x, SByteArray)) }
    forAll { t: SPredefType => testCollection(t) }
  }

  property("CollectionConstant serialization round trip") {
    testCollection(SBoolean)
    testCollection(SByte)
    testCollection(SShort)
    testCollection(SInt)
    testCollection(SLong)
    testCollection(SBigInt)
    testCollection(SGroupElement)
    testCollection(SUnit)
    testCollection(SBox)
    testCollection(SAvlTree)
  }

  private def caseObjectValue(v: SValue) = (v, Array[Byte](v.opCode))

  override def objects = Table(
    ("object", "bytes"),
    (FalseLeaf, Array[Byte](1, 0)),
    (TrueLeaf, Array[Byte](1, 1)),
    caseObjectValue(Height),
    caseObjectValue(Inputs),
    caseObjectValue(Outputs),
    caseObjectValue(LastBlockUtxoRootHash),
    caseObjectValue(Self),
    caseObjectValue(GroupGenerator),
    (LongConstant(1), Array[Byte](SLong.typeCode, 2)),
    (BigIntConstant(BigInteger.valueOf(0)), Array[Byte](SBigInt.typeCode, 1, 0)),
    (BigIntConstant(new BigInteger(Array[Byte](3,4,5))), Array[Byte](SBigInt.typeCode, 3, 3, 4, 5)),
    (ByteArrayConstant(Array[Byte](1, 3, 5, 9, 10, 100)), Array[Byte](ByteArrayTypeCode, 6, 1, 3, 5, 9, 10, 100)),
    (ByteArrayConstant(Array[Byte]()), Array[Byte](ByteArrayTypeCode, 0)),
    (ByteArrayConstant(Array[Byte](1)), Array[Byte](ByteArrayTypeCode, 1, 1)),
    (ByteArrayConstant(Array[Byte](1, 2, 3, 4, 5)), Array[Byte](ByteArrayTypeCode, 5, 1, 2, 3, 4, 5)),
    (BoolArrayConstant(Array[Boolean](true, false, true)), Array[Byte](BoolArrayTypeCode, 3, 5)) // bits: 00000101
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
