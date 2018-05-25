package sigmastate.serialization

import java.math.BigInteger
import org.ergoplatform._
import org.scalacheck.Arbitrary._
import sigmastate.Values.{FalseLeaf, Constant, SValue, TrueLeaf, BigIntConstant, GroupGenerator, IntConstant}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate._
import sigmastate.utils.Extensions._
import sigmastate.utils.ByteArrayBuilder

class ConstantSerializerSpecification extends TableSerializationSpecification {

  def roundtrip[T <: SType](c: Constant[T]) = {
    val b = new ByteArrayBuilder()
    b.appendValue(c)
    val bytes = b.toBytes
    val buf = Serializer.start(bytes, 0)
    val res = buf.getValue
    res shouldBe c
    val randomPrefix = arrayGen[Byte].sample.get
    val buf2 = Serializer.start(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = buf2.getValue
    res2 shouldBe c
  }

//  def testCollection[T <: SType](tpe: T) = {
//    implicit val wWrapped = wrappedTypeGen(tpe)
//    implicit val tag = tpe.classTag[T#WrappedType]
//    forAll { x: Array[T#WrappedType] => roundtrip[SCollection[T]](CollectionConstant(x)) }
//  }

  property("Constant serialization round trip") {
    forAll { x: Byte => roundtrip(Constant[SByte.type](x, SByte)) }
    //TODO unify representation of Boolean constants using Constant case class
//    forAll { x: Boolean => roundtrip(Constant[SBoolean.type](x, SBoolean)) }
    forAll { x: Long => roundtrip(Constant[SInt.type](x, SInt)) }
    forAll { x: BigInteger => roundtrip(Constant[SBigInt.type](x, SBigInt)) }
    forAll { x: EcPointType => roundtrip(Constant[SGroupElement.type](x, SGroupElement)) }
    forAll { x: ErgoBox => roundtrip(Constant[SBox.type](x, SBox)) }
    forAll { x: AvlTreeData => roundtrip(Constant[SAvlTree.type](x, SAvlTree)) }
//    forAll { x: Array[Byte] => roundtrip[SByteArray](x, SByteArray) }
//    forAll { t: SType => testCollection(t) }
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
    (IntConstant(1), Array[Byte]((OpCodes.ConstantCode + SInt.typeCode).toByte, 0, 0, 0, 0, 0, 0, 0, 1)),
    (BigIntConstant(BigInteger.valueOf(0)), Array[Byte](-107, 0, 1, 0)),
    (BigIntConstant(new BigInteger(Array[Byte](3,4,5))), Array[Byte](-107, 0, 3, 3, 4, 5))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
