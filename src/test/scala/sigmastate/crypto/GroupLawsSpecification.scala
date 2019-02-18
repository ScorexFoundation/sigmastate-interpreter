package sigmastate.crypto

import java.math.BigInteger

import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.CryptoConstants

class GroupLawsSpec extends SigmaTestingCommons {
  private val group = CryptoConstants.dlogGroup

  property("multiplication law is complete") {
    val identity = group.identity
    val ge = group.createRandomGenerator()

    group.multiplyGroupElements(ge, ge) shouldBe group.exponentiate(ge, new BigInteger("2"))
    group.multiplyGroupElements(ge, identity) shouldBe group.exponentiate(ge, BigInteger.ONE)
    group.multiplyGroupElements(ge, identity) shouldBe ge
    group.multiplyGroupElements(identity, identity) shouldBe identity

    val inverse = group.getInverse(ge)
    group.multiplyGroupElements(ge, inverse) shouldBe identity
  }

  property("exponentiation") {
    val identity = group.identity
    val ge = group.createRandomGenerator()

    group.exponentiate(ge, BigInteger.ZERO) shouldBe identity
    group.exponentiate(ge, BigInteger.ONE) shouldBe ge
    group.exponentiate(ge, group.order) shouldBe identity
    group.exponentiate(ge, group.order.add(BigInteger.ONE)) shouldBe ge
  }

}
