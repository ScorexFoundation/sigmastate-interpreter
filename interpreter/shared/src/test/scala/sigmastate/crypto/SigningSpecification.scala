package sigmastate.crypto

import org.scalacheck.Gen
import scorex.util.encode.Base16
import sigmastate.{AtLeast, COR, CAND}
import sigmastate.Values.SigmaBoolean
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.helpers.{ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter, TestingCommons}
import sigmastate.interpreter.{HintsBag, ContextExtension, ProverResult}
import sigmastate.serialization.transformers.ProveDHTupleSerializer
import sigmastate.crypto.ProveDHTuple

class SigningSpecification extends TestingCommons {

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

  property("ProveDHT signature test vector") {
    // test vector data is generated by prover and tested in signing_spec::sig_test_vector_prove_dht
    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val pdht = ProveDHTupleSerializer(ProveDHTuple.apply).fromBytes(Base16.decode(input = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980280c66feee88d56e47bf3f47c4109d9218c60c373a472a0d9537507c7ee828c4802a96f19e97df31606183c1719400682d1d40b1ce50c9a1ed1b19845e2b1b551bf0255ac02191cb229891fb1b674ea9df7fc8426350131d821fc4a53f29c3b1cb21a").get)
    val signature = Base16.decode("eba93a69b28cfdea261e9ea8914fca9a0b3868d50ce68c94f32e875730f8ca361bd3783c5d3e25802e54f49bd4fb9fafe51f4e8aafbf9815").get
    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    verifier.verify(pdht, fakeContext, proverResult, msg).get._1 shouldBe true
  }

  property("handle improper signature") {
    val pi = new ErgoLikeTestProvingInterpreter()
    val sigmaTree: SigmaBoolean = pi.publicKeys.head
    val verifier = new ErgoLikeTestInterpreter
    val msg = "any message".getBytes("UTF-8")
    val sig = "invalid signature".getBytes("UTF-8")
    verifier.verifySignature(sigmaTree, msg, sig)(null) shouldBe false
  }
  
  property("AND signature test vector") {
    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk1 = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val sk2 = DLogProverInput(BigInt("50415569076448343263191022044468203756975150511337537963383000142821297891310").bigInteger)
    val signature = Base16.decode("9b2ebb226be42df67817e9c56541de061997c3ea84e7e72dbb69edb7318d7bb525f9c16ccb1adc0ede4700a046d0a4ab1e239245460c1ba45e5637f7a2d4cc4cc460e5895125be73a2ca16091db2dcf51d3028043c2b9340").get
    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    val sigmaTree: SigmaBoolean = CAND(Seq(sk1.publicImage, sk2.publicImage))
    verifier.verify(sigmaTree, fakeContext, proverResult, msg).get._1 shouldBe true
  }
  
  property("OR signature test vector") {
    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk1 = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val sk2 = DLogProverInput(BigInt("50415569076448343263191022044468203756975150511337537963383000142821297891310").bigInteger)
    val signature = Base16.decode("ec94d2d5ef0e1e638237f53fd883c339f9771941f70020742a7dc85130aaee535c61321aa1e1367befb500256567b3e6f9c7a3720baa75ba6056305d7595748a93f23f9fc0eb9c1aaabc24acc4197030834d76d3c95ede60c5b59b4b306cd787d010e8217f34677d046646778877c669").get
    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    val sigmaTree: SigmaBoolean = COR(Seq(sk1.publicImage, sk2.publicImage))
    verifier.verify(sigmaTree, fakeContext, proverResult, msg).get._1 shouldBe true
  }

  property("OR with ProveDHT signature test vector") {
    // test vector data is generated by prover and tested in signing_spec::sig_test_vector_conj_or_prove_dht
    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk1 = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val pdht = ProveDHTupleSerializer(ProveDHTuple.apply).fromBytes(Base16.decode(input = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980214487635ebffa60b13a166bd0721c5f0ab603fc74168d7764d7ec5ef2107f5d40334c5b7efa5a4a22b83d102d2e6521eaa660fa911c5a213af63c8460f2327513b026a0be2a277291d42daad3830cb16a4ef20e4f1f7c36384f3fee065f0f143a355").get)
    val signature = Base16.decode("a80daebdcd57874296f49fd9910ddaefbf517ca076b6e16b97678e96a20239978836e7ec5b795cf3a55616d394f07c004f85e0d3e71880d4734b57ea874c7eba724e8887280f1affadaad962ee916b39207af2d2ab2a69a2e6f4d652f7389cc4f582bbe6d7937c59aa64cf2965a8b36a").get
    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    val sigmaTree: SigmaBoolean = COR(Seq(sk1.publicImage, pdht))
    verifier.verify(sigmaTree, fakeContext, proverResult, msg).get._1 shouldBe true
  }

  property("AND with OR signature test vector") {
    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk1 = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val sk2 = DLogProverInput(BigInt("50415569076448343263191022044468203756975150511337537963383000142821297891310").bigInteger)
    val sk3 = DLogProverInput(BigInt("34648336872573478681093104997365775365807654884817677358848426648354905397359").bigInteger)
    val signature = Base16.decode("397e005d85c161990d0e44853fbf14951ff76e393fe1939bb48f68e852cd5af028f6c7eaaed587f6d5435891a564d8f9a77288773ce5b526a670ab0278aa4278891db53a9842df6fba69f95f6d55cfe77dd7b4bdccc1a3378ac4524b51598cb813258f64c94e98c3ef891a6eb8cbfd2e527a9038ca50b5bb50058de55a859a169628e6ae5ba4cb0332c694e450782d6f").get
    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    val sigmaTree: SigmaBoolean = CAND(Seq(sk1.publicImage, COR(Seq(sk2.publicImage, sk3.publicImage))))
    verifier.verify(sigmaTree, fakeContext, proverResult, msg).get._1 shouldBe true
  }

  property("OR with AND signature test vector") {
    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk1 = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val sk2 = DLogProverInput(BigInt("50415569076448343263191022044468203756975150511337537963383000142821297891310").bigInteger)
    val sk3 = DLogProverInput(BigInt("34648336872573478681093104997365775365807654884817677358848426648354905397359").bigInteger)
    val signature = Base16.decode("a58b251be319a9656c21876b1136a59f42b18835dec6076c92f7a925ba28d2030218c177ab07563003eff5250cfafeb631ef610f4d710ab8e821bf632203adf23f4376580eaa17ddb36c0138f73a88551f45d92cde2b66dfbb5906c02e4d48106ff08be4a2fc29ec242f495468692f9ddeeb029dc5d8f38e2649cf09c44b67cbcfb3de4202026fb84d23ce2b4ff0f69b").get
    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    val proverResult = ProverResult(signature, ContextExtension.empty)
    val sigmaTree: SigmaBoolean = COR(Seq(sk1.publicImage, CAND(Seq(sk2.publicImage, sk3.publicImage))))
    verifier.verify(sigmaTree, fakeContext, proverResult, msg).get._1 shouldBe true
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
    verifier.verify(mkTestErgoTree(prop), fakeContext, proverResult, msg).get._1 shouldBe true

    // print one more random vector for debug purposes
    printThresholdSignature(msg)
  }

  property("signMessage / verifyMessage roundtrip - simple dlog") {
    forAll(Gen.alphaNumStr){str =>
      val msg = str.getBytes("UTF-8")
      val pi = new ErgoLikeTestProvingInterpreter()
      val sigmaTree: SigmaBoolean = pi.publicKeys.head
      val sig = pi.signMessage(sigmaTree, msg, HintsBag.empty).get
      pi.verifySignature(sigmaTree, msg, sig)(null) shouldBe true
      pi.verifySignature(sigmaTree, (str + "1").getBytes("UTF-8"), sig)(null) shouldBe false
      pi.verifySignature(sigmaTree, msg, sig :+ (1: Byte))(null) shouldBe true //possible to append bytes
      val wrongTree = pi.publicKeys(1)
      pi.verifySignature(wrongTree, msg, sig)(null) shouldBe false
    }
  }

  property("signMessage / verifyMessage roundtrip - complex key") {
    forAll(Gen.alphaNumStr){str =>
      val msg = str.getBytes("UTF-8")
      val pi = new ErgoLikeTestProvingInterpreter()
      val sigmaTree: SigmaBoolean = CAND(Seq(pi.dlogSecrets.head.publicImage, pi.dhSecrets.head.publicImage))
      val sig = pi.signMessage(sigmaTree, msg, HintsBag.empty).get
      pi.verifySignature(sigmaTree, msg, sig)(null) shouldBe true
      pi.verifySignature(sigmaTree, (str + "1").getBytes("UTF-8"), sig)(null) shouldBe false
      pi.verifySignature(sigmaTree, msg, sig :+ (1: Byte))(null) shouldBe true //possible to append bytes
      val wrongTree = CAND(Seq(pi.dlogSecrets.head.publicImage, pi.dhSecrets(1).publicImage))
      pi.verifySignature(wrongTree, msg, sig)(null) shouldBe false
    }
  }

  property("verifySignature w. simple signature test vector") {

    val msg = Base16.decode("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00").get
    val sk = DLogProverInput(BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746").bigInteger)
    val signature = Base16.decode("bcb866ba434d5c77869ddcbc3f09ddd62dd2d2539bf99076674d1ae0c32338ea95581fdc18a3b66789904938ac641eba1a66d234070207a2").get

    // check that signature is correct
    val verifier = new ErgoLikeTestInterpreter
    verifier.verifySignature(sk.publicImage, msg, signature)(null) shouldBe true

    // print one more random vector for debug purposes
    printSimpleSignature(msg: Array[Byte])
  }

  private def printSimpleSignature(msg: Array[Byte]) {
    val proverA = new ErgoLikeTestProvingInterpreter

    val sk = proverA.dlogSecrets.head
    val prop = sk.publicImage
    val tree = mkTestErgoTree(prop)
    val prove = proverA.prove(tree, fakeContext, msg).get

    printDebug(s"Message: ${Base16.encode(msg)}")
    printDebug(s"sk: ${sk.w}")
    printDebug(s"sk(Base16): ${Base16.encode(sk.w.toByteArray)}")
    printDebug(s"pkBytes: ${Base16.encode(prop.pkBytes)}")
    printDebug(s"treeBytes: ${Base16.encode(tree.bytes)}")
    printDebug(s"Signature: ${Base16.encode(prove.proof)}")
  }

  private def printThresholdSignature(msg: Array[Byte]) {
    val proverA = new ErgoLikeTestProvingInterpreter

    val sk1 = proverA.dlogSecrets.head
    val sk2 = proverA.dlogSecrets(1)
    val sk3 = DLogProverInput.random()


    val prop = AtLeast(2, sk1.publicImage, sk2.publicImage, sk3.publicImage)
    val prove = proverA.prove(mkTestErgoTree(prop), fakeContext, msg).get

    printDebug(s"Message: ${Base16.encode(msg)}")
    printDebug(s"sk1: ${sk1.w}")
    printDebug(s"sk2: ${sk2.w}")
    printDebug(s"sk3: ${sk3.w}")
    printDebug(s"Signature: ${Base16.encode(prove.proof)}")
  }

}
