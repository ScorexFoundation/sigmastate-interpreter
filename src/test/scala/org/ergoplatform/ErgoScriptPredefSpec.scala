package org.ergoplatform

import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.AvlTreeData
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.utxo.ErgoLikeTestInterpreter

import scala.util.Try

class ErgoScriptPredefSpec extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)

  property("tokenThreshold") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val tokenId: Digest32 = Blake2b256("id")
    val wrongId: Digest32 = Blake2b256(tokenId)
    val wrongId2: Digest32 = Blake2b256(wrongId)
    val tokenAmount: Int = 50

    val prop = ErgoScriptPredef.tokenThreshold(tokenId, tokenAmount)

    def check(inputBoxes:IndexedSeq[ErgoBox]): Try[Unit] = Try {
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
    check(IndexedSeq(ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount), (wrongId2, tokenAmount)), Map()))) shouldBe 'success

    // transaction with the only input with insufficient token should fail
    check(IndexedSeq(ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount - 1)), Map()))) shouldBe 'failure

    // transaction with multiple inputs with insufficient token should fail
    val inputs = IndexedSeq(
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount - 2)), Map()),
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount)), Map()),
      ErgoBox(20, prop, 0, Seq((tokenId, 1), (wrongId2, tokenAmount)), Map())
    )
    check(inputs) shouldBe 'failure

    // transaction with multiple inputs with enough token should pass
    val inputs2 = IndexedSeq(
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount), (tokenId, tokenAmount)), Map()),
      ErgoBox(20, prop, 0, Seq((wrongId, tokenAmount)), Map()),
      ErgoBox(20, prop, 0, Seq((tokenId, 1), (wrongId2, tokenAmount)), Map())
    )
    check(inputs2) shouldBe 'success

  }

}
