package sigmastate

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.crypto.hash.Blake2b256


class SchnorrSignatureSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  ignore("sign-verify roundtrip") {
    forAll() { (message: Array[Byte], modifier: Byte) =>

      import sigmastate.interpreter.GroupSettings.soundness

      val pi = DLogProverInput.random()
      val ci = pi.publicImage

      val pi2 = DLogProverInput.random()
      val ci2 = pi2.publicImage

      val challenge = Blake2b256(message)

      val sig = SchnorrSigner.generate(pi).prove(challenge)

      /* todo: fix the test
      sig.verify() shouldBe true

      val sigWrong1 = SchnorrNode(sig.proposition, message ++ Array(modifier), sig.signature).verify()
      val sigWrong2 = SchnorrNode(sig.proposition, Array(modifier) ++ message, sig.signature).verify()
      val sigWrong3 = SchnorrNode(ci2, message, sig.signature).verify()
      val sigWrong4 = SchnorrNode(sig.proposition, Blake2b256(message ++ Array(modifier)), sig.signature).verify()

      sigWrong1 shouldBe false
      sigWrong2 shouldBe false
      sigWrong3 shouldBe false
      sigWrong4 shouldBe false */
    }
  }
}
