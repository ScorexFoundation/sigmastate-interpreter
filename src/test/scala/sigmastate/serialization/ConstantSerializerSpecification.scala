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

  def roundtrip[T <: SType](c: Constant[T]) = {
    val bytes = Serializer.startWriter()
        .putValue(c)
        .toBytes
    val r = Serializer.startReader(bytes, 0)
    val res = r.getValue
    res shouldBe c
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = Serializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = r2.getValue
    res2 shouldBe c
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { x: Array[T#WrappedType] => roundtrip(Constant[SCollection[T]](x, SCollection(tpe))) }
  }

  property("Constant serialization round trip") {
    forAll { x: Byte => roundtrip(Constant[SByte.type](x, SByte)) }
    //TODO unify representation of Boolean constants using Constant case class
//    forAll { x: Boolean => roundtrip(Constant[SBoolean.type](x, SBoolean)) }
    forAll { x: Long => roundtrip(Constant[SLong.type](x, SLong)) }
    forAll { x: BigInteger => roundtrip(Constant[SBigInt.type](x, SBigInt)) }
    forAll { x: EcPointType => roundtrip(Constant[SGroupElement.type](x, SGroupElement)) }
    forAll { x: ErgoBox => roundtrip(Constant[SBox.type](x, SBox)) }
    forAll { x: AvlTreeData => roundtrip(Constant[SAvlTree.type](x, SAvlTree)) }
    forAll { x: Array[Byte] => roundtrip(Constant[SByteArray](x, SByteArray)) }
    forAll { t: SPrimType => testCollection(t) }
  }

  def caseObjectValue(v: SValue) = (v, Array[Byte](v.opCode))

  override def objects = Table(
    ("object", "bytes"),
    caseObjectValue(FalseLeaf),
    caseObjectValue(TrueLeaf),
    caseObjectValue(Height),
    caseObjectValue(Inputs),
    caseObjectValue(Outputs),
    caseObjectValue(LastBlockUtxoRootHash),
    caseObjectValue(Self),
    caseObjectValue(GroupGenerator),
    (LongConstant(1), Array[Byte](SLong.typeCode, 0, 0, 0, 0, 0, 0, 0, 1)),
    (BigIntConstant(BigInteger.valueOf(0)), Array[Byte](SBigInt.typeCode, 0, 1, 0)),
    (BigIntConstant(new BigInteger(Array[Byte](3,4,5))), Array[Byte](SBigInt.typeCode, 0, 3, 3, 4, 5)),
    (ByteArrayConstant(Array[Byte](1, 3, 5, 9, 10, 100)), Array[Byte](ByteArrayTypeCode, 0, 6, 1, 3, 5, 9, 10, 100)),
    (ByteArrayConstant(Array[Byte]()), Array[Byte](ByteArrayTypeCode, 0, 0)),
    (ByteArrayConstant(Array[Byte](1)), Array[Byte](ByteArrayTypeCode, 0, 1, 1)),
    (ByteArrayConstant(Array[Byte](1, 2, 3, 4, 5)), Array[Byte](ByteArrayTypeCode, 0, 5, 1, 2, 3, 4, 5))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
