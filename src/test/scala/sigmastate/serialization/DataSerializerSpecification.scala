package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import scalan.RType
import sigmastate.SCollection.SByteArray
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.eval.Evaluation
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.sigma.AvlTree

class DataSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val w = SigmaSerializer.startWriter()
    DataSerializer.serialize(obj, tpe, w)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes, 0)
    val res = DataSerializer.deserialize(tpe, r)
    res shouldBe obj
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = SigmaSerializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = DataSerializer.deserialize(tpe, r2)
    res2 shouldBe obj
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tAny = RType.AnyType
    forAll { xs: Array[T#WrappedType] =>
      roundtrip[SCollection[T]](xs.toColl, SCollection(tpe))
      roundtrip[SCollection[STuple]](xs.toColl.map(x => Colls.fromItems[Any](x, x)), SCollection(STuple(tpe, tpe)))
      roundtrip[SCollection[SCollection[T]]](xs.toColl.map(x => Colls.fromItems[T#WrappedType](x, x)), SCollection(SCollection(tpe)))
      roundtrip[SCollection[STuple]](
        xs.toColl.map { x =>
          val arr = Colls.fromItems[T#WrappedType](x, x)
          Colls.fromItems[Any](arr, arr)
        },
        SCollection(STuple(SCollection(tpe), SCollection(tpe)))
      )
    }
  }

  def testTuples[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    implicit val tAny = RType.AnyType
    forAll { in: (T#WrappedType, T#WrappedType) =>
      val (x,y) = (in._1, in._2)
      roundtrip[STuple](Colls.fromItems[Any](x, y), STuple(tpe, tpe))
      roundtrip[STuple](Colls.fromItems[Any](x, y, Colls.fromItems[Any](x, y)), STuple(tpe, tpe, STuple(tpe, tpe)))
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
    forAll { x: AvlTree => roundtrip[SAvlTree.type](x, SAvlTree) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x.toColl, SByteArray) }
    forAll { t: SPredefType => testCollection(t) }
    forAll { t: SPredefType => testTuples(t) }
  }

}
