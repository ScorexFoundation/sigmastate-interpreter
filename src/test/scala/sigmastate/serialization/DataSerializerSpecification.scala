package sigmastate.serialization

import java.math.BigInteger
import java.nio.ByteBuffer
import org.scalacheck.Arbitrary._
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate._
import sigmastate.utils.ByteArrayBuilder
import sigmastate.utxo.ErgoBox

class DataSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val b = new ByteArrayBuilder()
    DataSerializer.serialize(obj, tpe, b)
    val buf = ByteBuffer.wrap(b.toBytes)
    val res = DataSerializer.deserialize(tpe, buf)
    res shouldBe obj
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { x: Array[T#WrappedType] => roundtrip[SCollection[T]](x, SCollection(tpe)) }
  }

  property("Data serialization round trip") {
    forAll { x: Byte => roundtrip[SByte.type](x, SByte) }
    forAll { x: Boolean => roundtrip[SBoolean.type](x, SBoolean) }
    forAll { x: Long => roundtrip[SInt.type](x, SInt) }
    forAll { x: BigInteger => roundtrip[SBigInt.type](x, SBigInt) }
    forAll { x: EcPointType => roundtrip[SGroupElement.type](x, SGroupElement) }
    forAll { x: ErgoBox => roundtrip[SBox.type](x, SBox) }
    forAll { x: AvlTreeData => roundtrip[SAvlTree.type](x, SAvlTree) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x, SByteArray) }
    forAll { t: SType => testCollection(t) }
  }

}
