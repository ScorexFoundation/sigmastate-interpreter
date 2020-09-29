package sigmastate.utxo

import org.ergoplatform.Height
import org.scalacheck.Gen
import sigmastate.Values.IntConstant
import sigmastate._
import sigmastate.eval.IRContextFactoryImpl
import sigmastate.lang.Terms._
import sigmastate.helpers.{ErgoLikeTransactionTesting, ErgoLikeContextTesting, ErgoLikeTestInterpreter, SigmaTestingCommons, ContextEnrichingTestProvingInterpreter}

import scala.util.Random

class ComplexSigSpecification extends SigmaTestingCommons {
  def createIR = new TestingIRContext
  implicit lazy val IR: TestingIRContext = createIR
  implicit lazy val irFactory = new IRContextFactoryImpl(createIR)

  private def proverGen: Gen[ContextEnrichingTestProvingInterpreter] = for {
    _ <- Gen.const(1)
  } yield new ContextEnrichingTestProvingInterpreter()

  private def proversGen(min: Int, max: Int): Gen[Seq[ContextEnrichingTestProvingInterpreter]] =
    Gen.listOfN(Gen.chooseNum(min, max).sample.get, proverGen)

  /**
    * Whether A or B, or both are able to sign a transaction
    */
  property("simplest linear-sized ring signature (1-out-of-2 OR)") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """pubkeyA || pubkeyB""").asSigmaProp

    val prop = SigmaOr(pubkeyA, pubkeyB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prB, fakeMessage).get._1 shouldBe true

    proverC.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
  }

  property("simplest linear-sized ring signature (1-out-of-3 OR), with anyOf syntax") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """anyOf(Coll(pubkeyA, pubkeyB, pubkeyC))""").asSigmaProp

    val prop = SigmaOr(pubkeyA, pubkeyB, pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prB, fakeMessage).get._1 shouldBe true

    val prC = proverC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prC, fakeMessage).get._1 shouldBe true
  }

  property("simplest linear-sized ring signature (1-out-of-3 OR), with || syntax") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """pubkeyA || pubkeyB || pubkeyC""").asSigmaProp

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prB, fakeMessage).get._1 shouldBe true

    val prC = proverC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prC, fakeMessage).get._1 shouldBe true
  }

  //two secrets are known, nevertheless, one will be simulated
  property("simplest linear-sized ring signature (1-out-of-4 OR), all secrets are known") {
    val proverA = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA1 = proverA.dlogSecrets(0).publicImage
    val pubkeyA2 = proverA.dlogSecrets(1).publicImage
    val pubkeyA3 = proverA.dlogSecrets(2).publicImage
    val pubkeyA4 = proverA.dlogSecrets(3).publicImage

    val env = Map("pubkeyA1" -> pubkeyA1, "pubkeyA2" -> pubkeyA2, "pubkeyA3" -> pubkeyA3, "pubkeyA4" -> pubkeyA4)
    val compiledProp = compile(env, """anyOf(Coll(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))""").asSigmaProp

    val prop = SigmaOr(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of two ANDs") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """pubkeyA && pubkeyB || pubkeyC && pubkeyD""").asSigmaProp

    val prop = SigmaOr(SigmaAnd(pubkeyA, pubkeyB), SigmaAnd(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr, fakeMessage).get._1 shouldBe true

    val proverCD = proverC.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverCD.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of AND and OR") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """pubkeyA && pubkeyB || (pubkeyC || pubkeyD)""").asSigmaProp

    val prop = SigmaOr(SigmaAnd(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val prC = proverC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prC, fakeMessage).get._1 shouldBe true

    val prD = proverD.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prD, fakeMessage).get._1 shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("simple sig scheme - AND of two") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """pubkeyA && pubkeyB""").asSigmaProp

    val prop = SigmaAnd(pubkeyA, pubkeyB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of AND and DLOG") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """(pubkeyA && pubkeyB) || pubkeyC""").asSigmaProp

    val prop = SigmaOr(SigmaAnd(pubkeyA, pubkeyB), pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(compiledProp, ctx, fakeMessage).isSuccess shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(compiledProp, ctx, fakeMessage).get
    println("proof size: " + pr.proof.length)
    verifier.verify(compiledProp, ctx, pr, fakeMessage).get._1 shouldBe true

    val pr2 = proverC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - AND of two ORs") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA || pubkeyB) && (pubkeyC || pubkeyD)""").asSigmaProp

    val prop = SigmaAnd(SigmaOr(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val proverAC = proverA.withSecrets(Seq(proverC.dlogSecrets.head))
    val pr = proverAC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr, fakeMessage).get._1 shouldBe true

    val proverBD = proverB.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverBD.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - AND of AND and OR") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA && pubkeyB) && (pubkeyC || pubkeyD)""").asSigmaProp

    val prop = SigmaAnd(SigmaAnd(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    proverAB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val proverABC = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABC = proverABC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prABC, fakeMessage).get._1 shouldBe true

    val proverABD = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABD = proverABD.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prABD, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of two ORs") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter
    val proverD = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA || pubkeyB) || (pubkeyC || pubkeyD)""").asSigmaProp

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prB, fakeMessage).get._1 shouldBe true

    val prC = proverC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prC, fakeMessage).get._1 shouldBe true

    val prD = proverD.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prD, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR w. predicate") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """anyOf(Coll(pubkeyA, pubkeyB, HEIGHT > 500))""").asSigmaProp

    // rewritten by http://github.com/aslesarenko/sigma/blob/2740b51c86bdf1917f688d4ccdb1a0eae9755e0c/sigma-library/src/main/scala/scalan/SigmaLibrary.scala#L91
    val prop = SigmaOr(GT(Height, IntConstant(500)).toSigmaProp, SigmaOr(pubkeyA, pubkeyB))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)
    val prA = proverA.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(compiledProp, ctx1, fakeMessage).isFailure shouldBe true

    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)
    val prC = proverC.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prC, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate, parentheses") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env,
      """anyOf(Coll(pubkeyA || pubkeyB, pubkeyC && HEIGHT > 500))""").asSigmaProp

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), SigmaAnd(pubkeyC, GT(Height, IntConstant(500)).toSigmaProp))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(compiledProp, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA2 = proverA.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prA2, fakeMessage).get._1 shouldBe true
    val prB2 = proverB.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prB2, fakeMessage).get._1 shouldBe true
    val prC2 = proverC.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prC2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate, no parentheses") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter
    val proverC = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """pubkeyA || pubkeyB ||  (pubkeyC && HEIGHT > 500)""").asSigmaProp

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), SigmaAnd(pubkeyC, GT(Height, IntConstant(500)).toSigmaProp))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(compiledProp, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA2 = proverA.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prA2, fakeMessage).get._1 shouldBe true
    val prB2 = proverB.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prB2, fakeMessage).get._1 shouldBe true
    val prC2 = proverC.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prC2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - k-out-of-n threshold") {

    // disable scalacheck shrinking otherwise other constraints start to fail
    import org.scalacheck.Shrink
    implicit val noShrink: Shrink[Seq[ContextEnrichingTestProvingInterpreter]] = Shrink.shrinkAny

    forAll(proversGen(3, 5), minSuccessful(10 /*test is heavy*/)) { allProvers =>
      val verifier = new ErgoLikeTestInterpreter
      val k = Gen.chooseNum(2, allProvers.length - 1).sample.get
      val kNumKeysCombinations = allProvers.map(_.dlogSecrets.head).toSet
        .subsets
        .map(_.toSeq)
        .toSeq
        .filter(_.length == k)

      val prop = COR(
        kNumKeysCombinations.map(combs => CAND(combs.map(_.publicImage)))
      )

      val ctx = ErgoLikeContextTesting(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(fakeSelf),
        spendingTransaction = ErgoLikeTransactionTesting.dummy,
        self = fakeSelf)

      // any prover alone (no added secrets) should fail
      allProvers.foreach(_.prove(prop, ctx, fakeMessage).isFailure shouldBe true)

      for (_ <- 1 to 3) {
        val shuffledProvers = Random.shuffle(allProvers)
        val prover = shuffledProvers.head
        val neededExtraSecrets = k - 1 // prover already has one
        for (i <- 1 until neededExtraSecrets) {
          // provers with less than k secrets should fail
          prover.withSecrets(shuffledProvers.takeRight(i).map(_.dlogSecrets.head))
            .prove(prop, ctx, fakeMessage).isFailure shouldBe true
        }
        // prover with exactly k secrets should succeed
        val proverWithKSecrets = prover.withSecrets(
          shuffledProvers.takeRight(neededExtraSecrets).map(_.dlogSecrets.head))
        val prTry = proverWithKSecrets.prove(prop, ctx, fakeMessage)
        prTry shouldBe 'success
        verifier.verify(prop, ctx, prTry.get, fakeMessage).get._1 shouldBe true
      }
    }
  }

  property("nested thresholds") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val secret1 = prover.dlogSecrets.head
    val secret2 = prover.dlogSecrets(1)
    val secret3 = prover.dlogSecrets(2)
    val secret4 = prover.dlogSecrets(3)

    val pdlog1 = secret1.publicImage
    val pdlog2 = secret2.publicImage
    val pdlog3 = secret3.publicImage
    val pdlog4 = secret4.publicImage

    val otherProver = new ContextEnrichingTestProvingInterpreter

    val unknownSecret1 = otherProver.dlogSecrets.head
    val unknownSecret2 = otherProver.dlogSecrets(1)
    val unknownSecret3 = otherProver.dlogSecrets(2)

    val unknownPdlog1 = unknownSecret1.publicImage
    val unknownPdlog2 = unknownSecret2.publicImage
    val unknownPdlog3 = unknownSecret3.publicImage

    val c1 = CTHRESHOLD(2, Seq(pdlog1, pdlog2, unknownPdlog1))
    val c2 = CTHRESHOLD(2, Seq(pdlog3, pdlog4, unknownPdlog2))
    val c3 = CTHRESHOLD(2, Seq(unknownPdlog1, unknownPdlog2, unknownPdlog3))

    val prop = CTHRESHOLD(2, Seq(c1, c2, c3))

    val ctx = fakeContext

    val pr = prover.prove(prop, ctx, fakeMessage).get

    otherProver.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    verifier.verify(prop, ctx, pr, fakeMessage).isSuccess shouldBe true
  }

}
