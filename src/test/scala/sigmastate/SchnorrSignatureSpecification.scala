package sigmastate

import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.util.BigIntegers
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import sigmastate.SchnorrSignature


class SchnorrSignatureSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("sign-verify roundtrip") {
    forAll() { (message: Array[Byte]) =>
      val dlog = SchnorrSignature.dlogGroup

      val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
      val secret: BigInteger = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom())

      val sig = SchnorrSignature.sign(secret, message)

      sig.verify(SchnorrSignature.proposition(secret), message) shouldBe true
    }
  }

}
