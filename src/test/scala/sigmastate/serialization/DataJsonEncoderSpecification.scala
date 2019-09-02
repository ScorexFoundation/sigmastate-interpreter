package sigmastate.serialization


import java.math.BigInteger

import io.circe.Json
import org.scalacheck.Arbitrary._
import scalan.RType
import sigmastate.SCollection.SByteArray
import sigmastate.SType.AnyOps
import sigmastate._
import sigmastate.eval.Extensions._
import sigmastate.eval.{Evaluation, _}

class DataJsonEncoderSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val json = DataJsonEncoder.encode(obj, tpe)
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
      roundtrip[SType](((x, y), (x, y)).asWrappedType, STuple(STuple(tpe, tpe), STuple(tpe, tpe)))
      roundtrip[SType](((x, y), ((x, y), (x, y))).asWrappedType, STuple(STuple(tpe, tpe), STuple(STuple(tpe, tpe), STuple(tpe, tpe))))
    }
  }

  property("Data Json serialization round trip") {
    forAll { x: Byte => roundtrip[SByte.type](x, SByte) }
    forAll { x: Boolean => roundtrip[SBoolean.type](x, SBoolean) }
    forAll { x: Long => roundtrip[SLong.type](x, SLong) }
    forAll { x: String => roundtrip[SString.type](x, SString) }
    forAll { x: BigInteger => roundtrip[SBigInt.type](x, SBigInt) }
    forAll { x: Array[Byte] => roundtrip[SByteArray](x.toColl, SByteArray) }
    forAll { x: Option[Byte] => roundtrip[SOption[SByte.type]](x, SOption[SByte.type]) }
    testCollection(SByte)
    testCollection(SShort)
    testCollection(SInt)
    testCollection(SLong)
    testCollection(SBigInt)
    testCollection(SOption[SLong.type])
    testTuples(SByte)
    testTuples(SShort)
    testTuples(SInt)
    testTuples(SLong)
    testTuples(SBigInt)
    testTuples(SOption[SLong.type])
    //    forAll { t: SPredefType => testTuples(t) }
  }

  property("Example test") {
    def toUnifiedString(from: String): String = from.toString().replaceAll("[\n ]", "")

    toUnifiedString(DataJsonEncoder.encode((10, 20).asWrappedType, STuple(SInt, SInt)).toString()) shouldBe
      toUnifiedString("{ \"type\": \"(Int, Int)\", \"value\": { \"_1\": 10, \"_2\": 20 }}")
    toUnifiedString(DataJsonEncoder.encode(SigmaDsl.Colls.fromItems(1.toByte, 2.toByte, 3.toByte).asWrappedType, SCollectionType(SByte)).toString()) shouldBe
      toUnifiedString("{ \"type\": \"Coll[Byte]\", \"value\": [1, 2, 3] }")
    toUnifiedString(DataJsonEncoder.encode(SigmaDsl.Colls.fromItems((1, 10), (2, 20), (3, 30)).asWrappedType, SCollectionType(STuple(SInt, SInt))).toString()) shouldBe
      toUnifiedString("{ \"type\": \"Coll[(Int, Int)]\", \"value\": { \"_1\": [1, 2, 3], \"_2\": [10, 20, 30] }}")
    toUnifiedString(DataJsonEncoder.encode((SigmaDsl.Colls.fromItems((1, SigmaDsl.Colls.replicate(3, 1.toByte)), (2, SigmaDsl.Colls.replicate(2, 2.toByte)), (3, SigmaDsl.Colls.replicate(1, 3.toByte)), (4, SigmaDsl.Colls.replicate(0, 4.toByte))), 100.toLong).asWrappedType,
      STuple(SCollectionType(STuple(SInt, SCollectionType(SByte))), SLong)).toString()) shouldBe
      toUnifiedString(
        """
          |{ "type": "(Coll[(Int, Coll[Byte])], Long)",
          |  "value": {
          |     "_1": { "_1": [1, 2, 3, 4], "_2": [ [1, 1, 1], [2, 2], [3], [] ] },
          |     "_2": 100
          |  }
          |}
          |""".stripMargin)
  }
}
