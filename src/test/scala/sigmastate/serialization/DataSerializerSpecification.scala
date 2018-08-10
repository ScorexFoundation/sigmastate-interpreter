package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import sigmastate.SCollection.SByteArray
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.interpreter.CryptoConstants.EcPointType

class DataSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val w = Serializer.startWriter()
    DataSerializer.serialize(obj, tpe, w)
    val bytes = w.toBytes
    val r = Serializer.startReader(bytes, 0)
    val res = DataSerializer.deserialize(tpe, r)
    res shouldBe obj
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = Serializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = DataSerializer.deserialize(tpe, r2)
    res2 shouldBe obj
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { xs: Array[T#WrappedType] =>
      roundtrip[SCollection[T]](xs, SCollection(tpe))
      roundtrip[SCollection[STuple]](xs.map(x => Array[Any](x, x)), SCollection(STuple(tpe, tpe)))
      roundtrip[SCollection[SCollection[T]]](xs.map(x => Array[T#WrappedType](x, x)), SCollection(SCollection(tpe)))
      roundtrip[SCollection[STuple]](
        xs.map { x =>
          val arr = Array[T#WrappedType](x, x)
          Array[Any](arr, arr)
        },
        SCollection(STuple(SCollection(tpe), SCollection(tpe)))
      )
    }
  }

  def testTuples[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { in: (T#WrappedType, T#WrappedType) =>
      val (x,y) = (in._1, in._2)
      roundtrip[STuple](Array[Any](x, y), STuple(tpe, tpe))
      roundtrip[STuple](Array[Any](x, y, Array[Any](x, y)), STuple(tpe, tpe, STuple(tpe, tpe)))
    }
  }

  property("Data serialization round trip") {
    forAll { x: Byte => roundtrip[SByte.type](x, SByte) }
    forAll { x: Boolean => roundtrip[SBoolean.type](x, SBoolean) }
    forAll { x: Long => roundtrip[SLong.type](x, SLong) }
    forAll { x: String => roundtrip[SString.type](x, SString) }
    forAll { x: BigInteger => roundtrip[SBigInt.type](x, SBigInt) }
    forAll { x: EcPointType => roundtrip[SGroupElement.type](x, SGroupElement) }
    forAll { x: SigmaBoolean => roundtrip[SSigmaProp.type](x, SSigmaProp) }
    forAll { x: ErgoBox => roundtrip[SBox.type](x, SBox) }
    forAll { x: AvlTreeData => roundtrip[SAvlTree.type](x, SAvlTree) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x, SByteArray) }
    forAll { t: SPredefType => testCollection(t) }
    forAll { t: SPredefType => testTuples(t) }
  }

}
