package sigmastate

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.rework.DLogProtocol.DLogProverInput


class SchnorrSignatureSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("sign-verify roundtrip") {
    forAll() { (message: Array[Byte], modifier: Byte) =>

      import SchnorrSignature._

      val (pi, ci) = DLogProverInput.random()
      val (_, ci2) = DLogProverInput.random()

      val sig = SchnorrSignatureSigner(pi).sign(message)

      sig.verify() shouldBe true

      //todo: uncomment & fix
      //sig.verify(ci, message ++ Array(modifier)) shouldBe false
      //sig.verify(ci, Array(modifier) ++ message) shouldBe false
      //sig.verify(ci2, message) shouldBe false
    }
  }
}
