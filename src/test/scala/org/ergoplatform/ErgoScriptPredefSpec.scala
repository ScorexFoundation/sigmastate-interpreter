package org.ergoplatform

import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.AvlTreeData
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.utxo.ErgoLikeTestInterpreter
import scalan.util.BenchmarkUtil._

import scala.util.Try

class ErgoScriptPredefSpec extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
  }
  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)

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


    measure(10) { i =>
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
    /*
    Iter 0: 777 ms
    Iter 1: 353 ms
    Iter 2: 304 ms
    Iter 10: 110 ms
    Iter 30: 80 ms
    Iter 40: 72 ms
    Iter 60: 68 ms
    */
  }

}
