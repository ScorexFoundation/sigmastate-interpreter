package org.ergoplatform

import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.{ByIndex, ErgoLikeTestInterpreter, ExtractCreationInfo, SelectField}
import sigmastate.{AvlTreeData, EQ}

import scala.util.Try

class ErgoScriptPredefSpec extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = true
  }
  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)

  property("boxCreationHeight") {
    val verifier = new ErgoLikeTestInterpreter
    val prover = new ErgoLikeTestProvingInterpreter
    val minerProp = prover.dlogSecrets.head.publicImage
    val pk = minerProp.pkBytes

    val nextHeight = 1
    val prop = EQ(Height, ErgoScriptPredef.boxCreationHeight(ByIndex(Outputs, IntConstant(0))))
    val propInlined = EQ(Height, SelectField(ExtractCreationInfo(ByIndex(Outputs, IntConstant(0))), 1).asIntValue)
    prop shouldBe propInlined
    val inputBox = ErgoBox(1, prop, nextHeight, Seq(), Map())
    val inputBoxes = IndexedSeq(inputBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
    val minerBox = new ErgoBoxCandidate(1, minerProp, nextHeight, Seq(), Map())

    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(minerBox))

    val ctx = ErgoLikeContext(
      currentHeight = nextHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pk,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBox)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  ignore("feeProposition") {
    // TODO
  }

  property("emission specification") {
    val verifier = new ErgoLikeTestInterpreter
    val prover = new ErgoLikeTestProvingInterpreter
    val minerProp = prover.dlogSecrets.head.publicImage
    val pk = minerProp.pkBytes

    val genesisHeight = 1
    val fixedRatePeriod = 525600
    val epochLength = 64800
    val fixedRate = 7500000000L
    val oneEpochReduction = 300000000
    val minerRewardDelay = 720
    val coinsTotal = 9773992500000000L

    val prop = ErgoScriptPredef.emissionBoxProp(fixedRatePeriod, epochLength, oneEpochReduction, fixedRate, minerRewardDelay)

    val emissionBox = ErgoBox(coinsTotal, prop, 0, Seq(), Map(ErgoBox.nonMandatoryRegisters.head -> LongConstant(-1)))
    val inputBoxes = IndexedSeq(emissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    // collect fixedRate coins at the first step
    check(fixedRate, 1)

    def check(emissionAmount: Long, nextHeight: Int): Unit = {
      val newEmissionBox: ErgoBoxCandidate = new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop,
        nextHeight, Seq(), Map())
      val minerBox = new ErgoBoxCandidate(emissionAmount, minerProp, nextHeight, Seq(), Map())

      val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newEmissionBox, minerBox))

      val ctx = ErgoLikeContext(
        currentHeight = nextHeight,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = pk,
        boxesToSpend = inputBoxes,
        spendingTransaction,
        self = inputBoxes.head)
      val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
    }
  }

  property("tokenThreshold") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val tokenId: Digest32 = Blake2b256("id")
    val wrongId: Digest32 = Blake2b256(tokenId)
    val wrongId2: Digest32 = Blake2b256(wrongId)
    val tokenAmount: Int = 50

    val prop = ErgoScriptPredef.tokenThresholdScript(tokenId, tokenAmount)

    def check(inputBoxes: IndexedSeq[ErgoBox]): Try[Unit] = Try {
      val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))
      val amount = inputBoxes.map(_.value).sum
      val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(ErgoBox(amount, pubkey, 0)))

      val ctx = ErgoLikeContext(
        currentHeight = 50,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContext.dummyPubkey,
        boxesToSpend = inputBoxes,
        spendingTransaction,
        self = inputBoxes.head)

      val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
    }


    // transaction with the only input with enough token should pass
    val inputs0 = IndexedSeq(
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount), (wrongId2, tokenAmount)), Map())
    )
    check(inputs0) shouldBe 'success

    // transaction with the only input with insufficient token should fail
    val inputs1 = IndexedSeq(
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount - 1)), Map())
    )
    check(inputs1) shouldBe 'failure

    // transaction with multiple inputs with insufficient token should fail
    val inputs2 = IndexedSeq(
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount - 2)), Map()),
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount)), Map()),
      ErgoBox(20, prop, 0, Seq((tokenId, 1), (wrongId2, tokenAmount)), Map())
    )
    check(inputs2) shouldBe 'failure

    // transaction with multiple inputs with enough token should pass
    val inputs3 = IndexedSeq(
      ErgoBox(20, prop, 0, Seq((wrongId, 1), (tokenId, tokenAmount / 2)), Map()),
      ErgoBox(20, prop, 0, Seq((wrongId, 1)), Map()),
      ErgoBox(20, prop, 0, Seq((tokenId, tokenAmount / 2 + 1), (wrongId2, 1)), Map())
    )
    check(inputs3) shouldBe 'success

  }

}
