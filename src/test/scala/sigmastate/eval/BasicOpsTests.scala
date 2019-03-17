package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.scalatest.{FunSuite, Matchers}
import sigmastate.eval.Extensions._
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

  test("ByteArrayToBigInt should always produce a positive big int") {
    SigmaDsl.byteArrayToBigInt(collection[Byte](-1)).signum shouldBe 1
  }

  test("ByteArrayToBigInt should always produce big int less than dlog group order") {
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

  def test(f: Context => Boolean, ctx: Context, expected: Boolean) = {
    val contr = NoEnvContract(f)
    val res = contr.canOpen(ctx)
    res shouldBe expected
  }

  test("Coll.append")  {
    val c1 = collection[Byte](1, 2)
    val c2 = collection[Byte](3, 4)
    c1.append(c2).toArray shouldBe Array[Byte](1, 2, 3, 4)
  }

//  test("box.creationInfo._1 is Int") {
//    val box = newAliceBox(1, 100)
//    box.creationInfo._1 shouldBe a [Integer]
//  }


  case class Contract1(base64_pk1: String) extends SigmaContract {
    override def builder: SigmaDslBuilder = new TestSigmaDslBuilder
    override def canOpen(ctx: Context): Boolean = {
      val pk: SigmaProp = SigmaDsl.PubKey(base64_pk1)
      pk.isValid
    }
  }

  case class Contract2(base64_pkA: String, base64_pkB: String, base64_pkC: String) extends SigmaContract {
    override def builder: SigmaDslBuilder = new TestSigmaDslBuilder
    override def canOpen(ctx: Context): Boolean = {
      val pkA: SigmaProp = SigmaDsl.PubKey(base64_pkA)
      val pkB: SigmaProp = SigmaDsl.PubKey(base64_pkB)
      val pkC: SigmaProp = SigmaDsl.PubKey(base64_pkC)
      verifyZK(pkA || pkB || pkC)
    }
  }

  case class FriendContract(friend: Box) extends SigmaContract {
    override def builder: SigmaDslBuilder = new TestSigmaDslBuilder
    override def canOpen(ctx: Context): Boolean = {ctx.INPUTS.length == 2 && ctx.INPUTS(0).id == friend.id}
  }


}
