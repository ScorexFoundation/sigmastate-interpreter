package sigmastate.crypto

import scorex.util.encode.Base16
import sigmastate.AtLeast
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.helpers.{ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.{ContextExtension, ProverResult}

class SigningSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

  property("simple signature test vector") {

    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val signature = Base16.decode("bcb866ba434d5c77869ddcbc3f09ddd62dd2d2539bf99076674d1ae0c32338ea95581fdc18a3b66789904938ac641eba1a66d234070207a2").get

    // check that public key is correct
    Base16.encode(sk.publicImage.pkBytes) shouldBe "03cb0d49e4eae7e57059a3da8ac52626d26fc11330af8fb093fa597d8b93deb7b1"

    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    verifier.verify(sk.publicImage, fakeContext, proverResult, msg).get._1 shouldBe true

    // print one more random vector for debug purposes
    printSimpleSignature(msg: Array[Byte])
  }

  property("threshold signature test vector") {

    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk1 = DLogProverInput(BigInt("416167686186183758173232992934554728075978573242452195968805863126437865059").bigInteger)
    val sk2 = DLogProverInput(BigInt("34648336872573478681093104997365775365807654884817677358848426648354905397359").bigInteger)
    val sk3 = DLogProverInput(BigInt("50415569076448343263191022044468203756975150511337537963383000142821297891310").bigInteger)
    val signature = Base16.decode("0b6bf9bc42c7b509ab56c76318c0891b2c8d44ef5fafb1379cc6b72b89c53cd43f8ef10158ce08646301d09b450ea83a1cdbbfc3dc7438ece4bbe934919069c50ec5857209b0dbf120b325c88667bc84580720ff4b3c371ec752bc6874c933f7fa53fae411e65ae07b647d365caac8c6744276c04c0240dd55e1f62c0e17a093dd91493c68104b1e01a4069017668d3f").get

    // check that signature is correct
    val prop = AtLeast(2, sk1.publicImage, sk2.publicImage, sk3.publicImage)
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    verifier.verify(prop, fakeContext, proverResult, msg).get._1 shouldBe true

    // print one more random vector for debug purposes
    printThresholdSignature(msg)
  }

  private def printSimpleSignature(msg: Array[Byte]) {
    val proverA = new ErgoLikeTestProvingInterpreter

    val sk = proverA.dlogSecrets.head
    val prop = sk.publicImage
    val tree = prop.toSigmaProp.treeWithSegregation
    val prove = proverA.prove(tree, fakeContext, msg).get

    println(s"Message: ${Base16.encode(msg)}")
    println(s"sk: ${sk.w}")
    println(s"sk(Base16): ${Base16.encode(sk.w.toByteArray)}")
    println(s"pkBytes: ${Base16.encode(prop.pkBytes)}")
    println(s"treeBytes: ${Base16.encode(tree.bytes)}")
    println(s"Signature: ${Base16.encode(prove.proof)}")
  }


  private def printThresholdSignature(msg: Array[Byte]) {
    val proverA = new ErgoLikeTestProvingInterpreter

    val sk1 = proverA.dlogSecrets.head
    val sk2 = proverA.dlogSecrets(1)
    val sk3 = DLogProverInput.random()


    val prop = AtLeast(2, sk1.publicImage, sk2.publicImage, sk3.publicImage)
    val prove = proverA.prove(prop, fakeContext, msg).get

    println(s"Message: ${Base16.encode(msg)}")
    println(s"sk1: ${sk1.w}")
    println(s"sk2: ${sk2.w}")
    println(s"sk3: ${sk3.w}")
    println(s"Signature: ${Base16.encode(prove.proof)}")
  }

}
