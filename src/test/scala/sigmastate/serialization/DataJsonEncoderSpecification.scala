package sigmastate.serialization


import java.math.BigInteger
import java.nio.charset.StandardCharsets

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
import SType.AnyOps
import special.collection.Coll

class DataJsonEncoderSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val json = DataJsonEncoder.encode(obj, tpe)
    val str = json.toString.replaceAll("\n", "").replaceAll(" ", "").strip()
    val res = DataJsonEncoder.decode(json)
    res shouldBe obj
  }

  def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tT = Evaluation.stypeToRType(tpe)
    implicit val tagT = tT.classTag
    implicit val tAny = RType.AnyType
    forAll { xs: Array[T#WrappedType] =>
      roundtrip[SCollection[T]](xs.toColl, SCollection(tpe))
      roundtrip[SType](xs.toColl.map(x => (x, x)).asWrappedType, SCollection(STuple(tpe, tpe)))

      val triples = xs.toColl.map(x => TupleColl(x, x, x)).asWrappedType
      roundtrip(triples, SCollection(STuple(tpe, tpe, tpe)))

      val quartets = xs.toColl.map(x => TupleColl(x, x, x, x)).asWrappedType
      roundtrip(quartets, SCollection(STuple(tpe, tpe, tpe, tpe)))

      val nested = xs.toColl.map(x => Colls.fromItems[T#WrappedType](x, x))
      roundtrip[SCollection[SCollection[T]]](nested, SCollection(SCollection(tpe)))

      roundtrip[SType](
        xs.toColl.map { x =>
          val arr = Colls.fromItems[T#WrappedType](x, x)
          (arr, arr)
        }.asWrappedType,
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
      roundtrip[SType]((x, y).asWrappedType, STuple(tpe, tpe))
      roundtrip[SType](TupleColl(x, y, x).asWrappedType, STuple(tpe, tpe, tpe))
      roundtrip[SType](TupleColl(x, y, x, y).asWrappedType, STuple(tpe, tpe, tpe, tpe))
      roundtrip[STuple](Colls.fromItems[Any](x, y, (x, y)), STuple(tpe, tpe, STuple(tpe, tpe)))
      roundtrip[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, x)), STuple(tpe, tpe, STuple(tpe, tpe, tpe)))
      roundtrip[STuple](Colls.fromItems[Any](x, y, TupleColl(x, y, (x, y))), STuple(tpe, tpe, STuple(tpe, tpe, STuple(tpe, tpe))))
    }
  }

  property("Data Json serialization round trip") {
    forAll { x: Byte => roundtrip[SByte.type](x, SByte) }
    forAll { x: Boolean => roundtrip[SBoolean.type](x, SBoolean) }
    forAll { x: Long => roundtrip[SLong.type](x, SLong) }
    forAll { x: String => roundtrip[SString.type](x, SString) }
    forAll { x: BigInteger => roundtrip[SBigInt.type](x, SBigInt) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x.toColl, SByteArray) }
    testCollection(SByte)
    testCollection(SShort)
    testCollection(SInt)
    testCollection(SLong)
    testCollection(SBigInt)
    testTuples(SByte)
    testTuples(SShort)
    testTuples(SInt)
    testTuples(SLong)
    testTuples(SBigInt)
//    forAll { t: SPredefType => testTuples(t) }
  }
}
