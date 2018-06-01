package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter}
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}

class BasicOpsSpecification extends SigmaTestingCommons {

  def test(env: Map[String, Any], ext: Seq[(Byte, EvaluatedValue[_ <: SType])], script: String, propExp: Value[SBoolean.type]) = {
    val prover = new ErgoLikeProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = ext.toMap
    }

    val prop = compile(env, script).asBoolValue
    prop shouldBe propExp
//    val prop = propExp
    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxExt = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxExt, pr.proof, fakeMessage).get._1 shouldBe true
  }

  val intVar1 = 1.toByte
  val intVar2 = 2.toByte
  val byteVar1 = 3.toByte
  val byteVar2 = 4.toByte
  val bigIntVar1 = 5.toByte
  val bigIntVar2 = 6.toByte
  val bigIntVar3 = 7.toByte
  val byteVar3 = 8.toByte
  val ext = Seq(
    (intVar1, LongConstant(1)), (intVar2, LongConstant(2)),
    (byteVar1, ByteConstant(1)), (byteVar2, ByteConstant(2)),
    (bigIntVar1, BigIntConstant(BigInt(10).underlying())), (bigIntVar2, BigIntConstant(BigInt(20).underlying())))
  val env = Map(
    "intVar1" -> intVar1, "intVar2" -> intVar2,
    "byteVar1" -> byteVar1, "byteVar2" -> byteVar2, "byteVar3" -> byteVar3,
    "bigIntVar1" -> bigIntVar1, "bigIntVar2" -> bigIntVar2, "bigIntVar3" -> bigIntVar3)

  property("Relation operations") {
    test(env, ext,
      "{ getVar[Int](intVar2) > getVar[Int](intVar1) && getVar[Int](intVar1) < getVar[Int](intVar2) }",
      AND(GT(TaggedInt(intVar2), TaggedInt(intVar1)), LT(TaggedInt(intVar1), TaggedInt(intVar2)))
    )
    test(env, ext,
      "{ getVar[Int](intVar2) >= getVar[Int](intVar1) && getVar[Int](intVar1) <= getVar[Int](intVar2) }",
      AND(GE(TaggedInt(intVar2), TaggedInt(intVar1)), LE(TaggedInt(intVar1), TaggedInt(intVar2)))
    )
    test(env, ext,
      "{ getVar[Byte](byteVar2) > getVar[Byte](byteVar1) && getVar[Byte](byteVar1) < getVar[Byte](byteVar2) }",
      AND(GT(TaggedByte(byteVar2), TaggedByte(byteVar1)), LT(TaggedByte(byteVar1), TaggedByte(byteVar2)))
    )
    test(env, ext,
      "{ getVar[Byte](byteVar2) >= getVar[Byte](byteVar1) && getVar[Byte](byteVar1) <= getVar[Byte](byteVar2) }",
      AND(GE(TaggedByte(byteVar2), TaggedByte(byteVar1)), LE(TaggedByte(byteVar1), TaggedByte(byteVar2)))
    )
    test(env, ext,
      "{ getVar[BigInt](bigIntVar2) > getVar[BigInt](bigIntVar1) && getVar[BigInt](bigIntVar1) < getVar[BigInt](bigIntVar2) }",
      AND(GT(TaggedBigInt(bigIntVar2), TaggedBigInt(bigIntVar1)), LT(TaggedBigInt(bigIntVar1), TaggedBigInt(bigIntVar2)))
    )
    test(env, ext,
      "{ getVar[BigInt](bigIntVar2) >= getVar[BigInt](bigIntVar1) && getVar[BigInt](bigIntVar1) <= getVar[BigInt](bigIntVar2) }",
      AND(GE(TaggedBigInt(bigIntVar2), TaggedBigInt(bigIntVar1)), LE(TaggedBigInt(bigIntVar1), TaggedBigInt(bigIntVar2)))
    )
  }

  property("Arith operations") {
    test(env, ext,
      "{ getVar[Int](intVar2) * 2 + getVar[Int](intVar1) == 5 }",
      EQ(Plus(Multiply(TaggedInt(intVar2), LongConstant(2)), TaggedInt(intVar1)), LongConstant(5))
    )
    test(env, ext :+ (bigIntVar3 -> BigIntConstant(50)),
      "{ getVar[BigInt](bigIntVar2) * intToBigInt(2) + getVar[BigInt](bigIntVar1) == getVar[BigInt](bigIntVar3) }",
      EQ(Plus(Multiply(TaggedBigInt(bigIntVar2), IntToBigInt(LongConstant(2))), TaggedBigInt(bigIntVar1)), TaggedBigInt(bigIntVar3))
    )
    test(env, ext :+ (byteVar3 -> ByteConstant(5)),
      "{ getVar[Byte](byteVar2) * intToByte(2) + getVar[Byte](byteVar1) == intToByte(5) }",
      EQ(Plus(Multiply(TaggedByte(byteVar2), IntToByte(LongConstant(2))), TaggedByte(byteVar1)), IntToByte(LongConstant(5)))
    )
    test(env, ext,
      "{ getVar[Int](intVar2) / 2 + getVar[Int](intVar1) == 2 }",
      EQ(Plus(Divide(TaggedInt(intVar2), LongConstant(2)), TaggedInt(intVar1)), LongConstant(2))
    )
    test(env, ext,
      "{ getVar[Int](intVar2) % 2 + getVar[Int](intVar1) == 1 }",
      EQ(Plus(Modulo(TaggedInt(intVar2), LongConstant(2)), TaggedInt(intVar1)), LongConstant(1))
    )
  }

}
