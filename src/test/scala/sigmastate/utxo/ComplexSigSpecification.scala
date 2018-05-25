package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter, Height}
import sigmastate.Values.IntConstant
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}

class ComplexSigSpecification extends SigmaTestingCommons {

  /**
    * Whether A or B, or both are able to sign a transaction
    */
  property("simplest linear-sized ring signature (1-out-of-2 OR)") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """pubkeyA || pubkeyB""")

    val prop = OR(pubkeyA, pubkeyB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prB, fakeMessage).get._1 shouldBe true

    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
  }

  property("simplest linear-sized ring signature (1-out-of-3 OR)") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA, pubkeyB, pubkeyC))""")

    val prop = OR(pubkeyA, pubkeyB, pubkeyC)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prB, fakeMessage).get._1 shouldBe true

    val prC = proverC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prC, fakeMessage).get._1 shouldBe true
  }

  //two secrets are known, nevertheless, one will be simulated
  property("simplest linear-sized ring signature (1-out-of-4 OR), all secrets are known") {
    val proverA = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA1 = proverA.dlogSecrets(0).publicImage
    val pubkeyA2 = proverA.dlogSecrets(1).publicImage
    val pubkeyA3 = proverA.dlogSecrets(2).publicImage
    val pubkeyA4 = proverA.dlogSecrets(3).publicImage

    val env = Map("pubkeyA1" -> pubkeyA1, "pubkeyA2" -> pubkeyA2, "pubkeyA3" -> pubkeyA3, "pubkeyA4" -> pubkeyA4)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))""")

    val prop = OR(Seq(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of two ANDs") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val proverD = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """pubkeyA && pubkeyB || pubkeyC && pubkeyD""")

    val prop = OR(AND(pubkeyA, pubkeyB), AND(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    val proverCD = proverC.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverCD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of AND and OR") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val proverD = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """pubkeyA && pubkeyB || (pubkeyC || pubkeyD)""")

    val prop = OR(AND(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val prC = proverC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prC, fakeMessage).get._1 shouldBe true

    val prD = proverD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prD, fakeMessage).get._1 shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - AND of two ORs") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val proverD = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA || pubkeyB) && (pubkeyC || pubkeyD)""")

    val prop = AND(OR(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverAC = proverA.withSecrets(Seq(proverC.dlogSecrets.head))
    val pr = proverAC.prove(prop, ctx, fakeMessage).get
    println("proof size: " + SigSerializer.toBytes(pr.proof).length)
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    val proverBD = proverB.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverBD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr2, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - AND of AND and OR") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val proverD = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA && pubkeyB) && (pubkeyC || pubkeyD)""")

    val prop = AND(AND(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    proverAB.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverABC = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABC = proverABC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prABC, fakeMessage).get._1 shouldBe true

    val proverABD = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABD = proverABD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prABD, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of two ORs") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter
    val proverD = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA || pubkeyB) || (pubkeyC || pubkeyD)""")

    val prop = OR(OR(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true

    val prB = proverB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prB, fakeMessage).get._1 shouldBe true

    val prC = proverC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prC, fakeMessage).get._1 shouldBe true

    val prD = proverD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prD, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR w. predicate") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA, pubkeyB, HEIGHT > 500))""")

    val prop = OR(pubkeyA, pubkeyB, GT(Height, IntConstant(500)))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    val prA = proverA.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(prop, ctx1, fakeMessage).isFailure shouldBe true

    val ctx2 = ErgoLikeContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    val prC = proverC.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prC, fakeMessage).get._1 shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA || pubkeyB, pubkeyC && HEIGHT > 500))""")

    val prop = OR(OR(pubkeyA, pubkeyB), AND(pubkeyC, GT(Height, IntConstant(500))))
    compiledProp shouldBe prop

    val ctx1 = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prA, fakeMessage).get._1 shouldBe true
    val prB = proverB.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prB, fakeMessage).get._1 shouldBe true
    proverC.prove(prop, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = ErgoLikeContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA2 = proverA.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prA2, fakeMessage).get._1 shouldBe true
    val prB2 = proverB.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prB2, fakeMessage).get._1 shouldBe true
    val prC2 = proverC.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prC2, fakeMessage).get._1 shouldBe true
  }
}
