package sigmastate.crypto

import java.math.BigInteger

import org.scalacheck.Gen
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.util.Random

class GroupLawsSpecification extends SigmaTestingCommons {
  private val group = CryptoConstants.dlogGroup

  val groupElementGen: Gen[EcPointType] = Gen.const(group.createRandomElement())
  val groupGeneratorGen: Gen[EcPointType] = Gen.const(group.createRandomGenerator())
  val bigIntGen: Gen[BigInteger] = Gen.const{
    val bytes = Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte)
    new BigInteger(1, bytes).mod(group.order)
  }

  property("multiplication law is complete") {
    forAll(groupElementGen) { ge =>
      val identity = group.identity
      group.multiplyGroupElements(ge, ge) shouldBe group.exponentiate(ge, new BigInteger("2"))
      group.multiplyGroupElements(ge, identity) shouldBe group.exponentiate(ge, BigInteger.ONE)
      group.multiplyGroupElements(ge, identity) shouldBe ge
      group.multiplyGroupElements(identity, identity) shouldBe identity

      val inverse = group.inverseOf(ge)
      group.multiplyGroupElements(ge, inverse) shouldBe identity
    }
  }

  property("exponentiation") {
    val identity = group.identity
    forAll(groupElementGen) { ge =>
      group.exponentiate(ge, BigInteger.ZERO) shouldBe identity
      group.exponentiate(ge, BigInteger.ONE) shouldBe ge
      group.exponentiate(ge, group.order) shouldBe identity
      group.exponentiate(ge, group.order.add(BigInteger.ONE)) shouldBe ge
    }
  }

  property("double inverse") {
    forAll(groupElementGen) { ge =>
      group.inverseOf(group.inverseOf(ge)) shouldBe ge
    }
  }

  property("precomputed"){
    forAll(groupGeneratorGen, bigIntGen) { case (base, exp) =>
      group.exponentiateWithPreComputedValues(base, exp) shouldBe group.exponentiate(base, exp)
    }
  }

}
