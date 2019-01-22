package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, Height}
import org.scalacheck.Gen
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}

import scala.util.Random

class ComplexSigSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  private def proverGen: Gen[ErgoLikeTestProvingInterpreter] = for {
    _ <- Gen.const(1)
  } yield new ErgoLikeTestProvingInterpreter()

  private def proversGen(min: Int, max: Int): Gen[Seq[ErgoLikeTestProvingInterpreter]] =
    Gen.listOfN(Gen.chooseNum(min, max).sample.get, proverGen)

  /**
    * Whether A or B, or both are able to sign a transaction
    */
  property("simplest linear-sized ring signature (1-out-of-2 OR)") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compileWithCosting(env, """pubkeyA || pubkeyB""").asBoolValue

    val prop = SigmaOr(pubkeyA, pubkeyB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prB, fakeMessage).get._1 shouldBe true

    proverC.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
  }

  property("simplest linear-sized ring signature (1-out-of-3 OR), with anyOf syntax") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compileWithCosting(env, """anyOf(Coll(pubkeyA, pubkeyB, pubkeyC))""").asBoolValue

    val prop = SigmaOr(pubkeyA, pubkeyB, pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prB, fakeMessage).get._1 shouldBe true

    val prC = proverC.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prC, fakeMessage).get._1 shouldBe true
  }

  property("simplest linear-sized ring signature (1-out-of-3 OR), with || syntax") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compileWithCosting(env, """pubkeyA || pubkeyB || pubkeyC""").asBoolValue

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA1 = proverA.dlogSecrets(0).publicImage
    val pubkeyA2 = proverA.dlogSecrets(1).publicImage
    val pubkeyA3 = proverA.dlogSecrets(2).publicImage
    val pubkeyA4 = proverA.dlogSecrets(3).publicImage

    val env = Map("pubkeyA1" -> pubkeyA1, "pubkeyA2" -> pubkeyA2, "pubkeyA3" -> pubkeyA3, "pubkeyA4" -> pubkeyA4)
    val compiledProp = compileWithCosting(env, """anyOf(Coll(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))""").asBoolValue

    val prop = SigmaOr(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of two ANDs") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compileWithCosting(env, """pubkeyA && pubkeyB || pubkeyC && pubkeyD""").asBoolValue

    val prop = SigmaOr(SigmaAnd(pubkeyA, pubkeyB), SigmaAnd(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compileWithCosting(env, """pubkeyA && pubkeyB || (pubkeyC || pubkeyD)""").asBoolValue

    val prop = SigmaOr(SigmaAnd(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compileWithCosting(env, """pubkeyA && pubkeyB""").asBoolValue

    val prop = SigmaAnd(pubkeyA, pubkeyB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    proverA.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(compiledProp, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of AND and DLOG") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compileWithCosting(env, """(pubkeyA && pubkeyB) || pubkeyC""").asBoolValue

    val prop = SigmaOr(SigmaAnd(pubkeyA, pubkeyB), pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compileWithCosting(env, """(pubkeyA || pubkeyB) && (pubkeyC || pubkeyD)""").asBoolValue

    val prop = SigmaAnd(SigmaOr(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compileWithCosting(env, """(pubkeyA && pubkeyB) && (pubkeyC || pubkeyD)""").asBoolValue

    val prop = SigmaAnd(SigmaAnd(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compileWithCosting(env, """(pubkeyA || pubkeyB) || (pubkeyC || pubkeyD)""").asBoolValue

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), SigmaOr(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compileWithCosting(env, """anyOf(Coll(pubkeyA, pubkeyB, HEIGHT > 500))""").asBoolValue

    // rewritten by http://github.com/aslesarenko/sigma/blob/2740b51c86bdf1917f688d4ccdb1a0eae9755e0c/sigma-library/src/main/scala/scalan/SigmaLibrary.scala#L91
    val prop = SigmaOr(GT(Height, IntConstant(500)).toSigmaProp, SigmaOr(pubkeyA, pubkeyB))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)
    val prA = proverA.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(compiledProp, ctx1, fakeMessage).isFailure shouldBe true

    val ctx2 = ErgoLikeContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)
    val prC = proverC.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prC, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate, parentheses") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compileWithCosting(env,
      """anyOf(Coll(pubkeyA || pubkeyB, pubkeyC && HEIGHT > 500))""").asBoolValue

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), SigmaAnd(pubkeyC, GT(Height, IntConstant(500)).toSigmaProp))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(compiledProp, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = ErgoLikeContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA2 = proverA.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prA2, fakeMessage).get._1 shouldBe true
    val prB2 = proverB.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prB2, fakeMessage).get._1 shouldBe true
    val prC2 = proverC.prove(compiledProp, ctx2, fakeMessage).get
    verifier.verify(compiledProp, ctx2, prC2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate, no parentheses") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compileWithCosting(env, """pubkeyA || pubkeyB ||  (pubkeyC && HEIGHT > 500)""").asBoolValue

    val prop = SigmaOr(SigmaOr(pubkeyA, pubkeyB), SigmaAnd(pubkeyC, GT(Height, IntConstant(500)).toSigmaProp))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(compiledProp, ctx1, fakeMessage).get
    verifier.verify(compiledProp, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(compiledProp, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = ErgoLikeContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
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
    implicit val noShrink: Shrink[Seq[ErgoLikeTestProvingInterpreter]] = Shrink.shrinkAny

    forAll(proversGen(3, 6), minSuccessful(10 /*test is heavy*/)) { allProvers =>
      val verifier = new ErgoLikeTestInterpreter
      val k = Gen.chooseNum(2, allProvers.length - 1).sample.get
      val kNumKeysCombinations = allProvers.map(_.dlogSecrets.head).toSet
        .subsets
        .map(_.toSeq)
        .toSeq
        .filter(_.length == k)

      val prop = OR(
        kNumKeysCombinations.map(combs => AND(combs.map(_.publicImage.isProven)))
      )

      val ctx = ErgoLikeContext(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContext.dummyPubkey,
        boxesToSpend = IndexedSeq(fakeSelf),
        spendingTransaction = null,
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
        prTry.isSuccess shouldBe true
        verifier.verify(prop, ctx, prTry.get, fakeMessage).get._1 shouldBe true
      }
    }
  }
}
