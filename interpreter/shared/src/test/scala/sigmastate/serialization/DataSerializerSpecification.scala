package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import scalan.RType
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{SigmaBoolean, ErgoTree}
import sigmastate._
import sigmastate.eval.Evaluation
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.basics.CryptoConstants.EcPointType
import special.sigma.AvlTree
import SType.AnyOps
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import sigmastate.exceptions.SerializerException
import sigmastate.interpreter.{CostAccumulator, ErgoTreeEvaluator}
import sigmastate.interpreter.ErgoTreeEvaluator.DefaultProfiler
import sigmastate.utils.Helpers
import scorex.util.encode.Base16

class DataSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](obj: T#WrappedType, tpe: T) = {
    val w = SigmaSerializer.startWriter()
    DataSerializer.serialize(obj, tpe, w)
    val bytes = w.toBytes
    val r = SigmaSerializer.startReader(bytes)
    val res = DataSerializer.deserialize(tpe, r)
    res shouldBe obj

    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(ScriptCostLimit.value)))
    val evaluator = new ErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, DefaultProfiler, ErgoTreeEvaluator.DefaultEvalSettings)
    val ok = DataValueComparer.equalDataValues(res, obj)(evaluator)
    ok shouldBe true

    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = SigmaSerializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = DataSerializer.deserialize(tpe, r2)
    res2 shouldBe obj
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

  property("Should check limits and fail") {
    val len = 0xFFFF + 1
    val tup = SigmaDsl.Colls.replicate(len, 1.asInstanceOf[Any])(RType.AnyType)
    assertExceptionThrown({
        val w = SigmaSerializer.startWriter()
        DataSerializer.serialize(tup.asWrappedType, STuple(Array.fill(len)(SInt)), w)
      },
      { t =>
        t.isInstanceOf[RuntimeException] &&
        t.getMessage.contains(s"Length of tuple $len exceeds ${0xFFFF} limit.")
      })

    val tooBig = SigmaDsl.BigInt(new BigInteger(Helpers.decodeBytes(
      "80e0ff7f02807fff72807f0a00ff7fb7c57f75c11ba2802970fd250052807fc37f6480ffff007fff18eeba44").toArray))

    assertExceptionThrown({
      val w = SigmaSerializer.startWriter()
      DataSerializer.serialize(tooBig.asWrappedType, SBigInt, w)
      val r = SigmaSerializer.startReader(w.toBytes)
      DataSerializer.deserialize(SBigInt, r)
    },
    { t =>
      t.isInstanceOf[SerializerException] &&
          t.getMessage.contains(s"BigInt value doesn't not fit into ${SBigInt.MaxSizeInBytes} bytes")
    })

  }

  property("should deserialize 860202660263 as non-evaluated value(Tuple)") {
    // below is an expression and not a data value encoded.
    // 0x86 is the op code for Tuple expression following by 0x02 (size), 0x0266
    // (SByte type 0x02, value 0x66), 0x0263 (SByte type 0x02, value 0x63).
    // found in R8 in https://explorer.ergoplatform.com/en/transactions/b5fd96c9f8c1ff436d609a225a12377c6873b890a145fc05f4099845b89315e5
    val tuple_str = "860202660263"
    val tuple_bytes = Base16.decode(tuple_str).get
    val tuple_deser = ValueSerializer.deserialize(tuple_bytes)
    tuple_deser.isInstanceOf[Values.Tuple] shouldBe true
    tuple_deser.isInstanceOf[Values.EvaluatedValue[_]] shouldBe false
  }
}
