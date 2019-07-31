package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.scalatest.{FunSuite, Matchers}
import special.sigma.{Box, Context, ContractsTestkit, MockSigma, SigmaContract, SigmaDslBuilder, SigmaProp, TestSigmaDslBuilder}

import scala.language.implicitConversions

class BasicOpsTests extends FunSuite with ContractsTestkit with Matchers {
  override val SigmaDsl: SigmaDslBuilder = CostingSigmaDslBuilder

  implicit def boolToSigma(b: Boolean): SigmaProp = MockSigma(b)

  test("atLeast") {
    val props = Colls.fromArray(Array[SigmaProp](false, true, true, false))

    // border cases
    SigmaDsl.atLeast(0, props).isValid shouldBe true
    SigmaDsl.atLeast(5, props).isValid shouldBe false

    // normal cases
    SigmaDsl.atLeast(1, props).isValid shouldBe true
    SigmaDsl.atLeast(2, props).isValid shouldBe true
    SigmaDsl.atLeast(3, props).isValid shouldBe false
  }

  // TODO this is valid for BigIntModQ type (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/554)
  ignore("ByteArrayToBigInt should always produce a positive big int") {
    SigmaDsl.byteArrayToBigInt(collection[Byte](-1)).signum shouldBe 1
  }

  // TODO this is valid for BigIntModQ type (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/554)
  ignore("ByteArrayToBigInt should always produce big int less than dlog group order") {
    val groupOrder = CustomNamedCurves.getByName("secp256k1").getN

    SigmaDsl.byteArrayToBigInt(
      Colls.fromArray(groupOrder.subtract(BigInteger.ONE).toByteArray)
    ).compareTo(SigmaDsl.BigInt(BigInteger.ONE)) shouldBe 1

    SigmaDsl.byteArrayToBigInt(
      Colls.fromArray(groupOrder.toByteArray)
    ).compareTo(SigmaDsl.BigInt(BigInteger.ONE)) shouldBe 1

    an [RuntimeException] should be thrownBy
      SigmaDsl.byteArrayToBigInt(Colls.fromArray(groupOrder.add(BigInteger.ONE).toByteArray))

    an [RuntimeException] should be thrownBy
      SigmaDsl.byteArrayToBigInt(Colls.fromArray(Array.fill[Byte](500)(1)))
  }

  test("Coll.append")  {
    val c1 = collection[Byte](1, 2)
    val c2 = collection[Byte](3, 4)
    c1.append(c2).toArray shouldBe Array[Byte](1, 2, 3, 4)
  }

  test("box.creationInfo._1 is Int") {
    val box = newAliceBox(1, 100)
    box.creationInfo._1 shouldBe a [Integer]
  }

}
