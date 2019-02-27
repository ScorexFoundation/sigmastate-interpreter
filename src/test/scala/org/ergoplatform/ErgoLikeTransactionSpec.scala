package org.ergoplatform

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.util.Random
import sigmastate.Values.ByteArrayConstant
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.SigmaSerializer
import sigmastate.serialization.generators.ValueGenerators

class ErgoLikeTransactionSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ValueGenerators
  with SigmaTestingCommons {

  property("ErgoLikeTransaction: Serializer round trip") {
    forAll { t: ErgoLikeTransaction => roundTripTest(t)(ErgoLikeTransaction.serializer) }
    forAll { t: ErgoLikeTransaction => roundTripTestWithPos(t)(ErgoLikeTransaction.serializer) }
  }

  property("ErgoLikeTransaction with same token in different outputs : Serializer round trip") {
    forAll { txIn: ErgoLikeTransaction =>
      whenever(txIn.outputCandidates.head.additionalTokens.nonEmpty) {
        val out = txIn.outputCandidates.head
        val outputs = (0 until 10).map { i =>
          new ErgoBoxCandidate(out.value, out.ergoTree, i, out.additionalTokens, out.additionalRegisters)
        }
        val tx = new ErgoLikeTransaction(txIn.inputs, txIn.dataInputs, txIn.outputCandidates ++ outputs)
        roundTripTestWithPos(tx)(ErgoLikeTransaction.serializer)

        // check that token id is written only once
        val w = SigmaSerializer.startWriter()
        ErgoLikeTransaction.serializer.serialize(tx, w)
        val bytes = w.toBytes

        tx.outputCandidates.flatMap(_.additionalTokens).foreach { token =>
          bytes.indexOfSlice(token._1) should not be -1
          bytes.indexOfSlice(token._1) shouldBe bytes.lastIndexOfSlice(token._1)
        }
      }
    }
  }

  property("ErgoLikeTransaction fields modification should change messageToSign") {
    forAll { tx0: ErgoLikeTransaction =>
      // generate transaction with same token in different outputs
      val headOut = tx0.outputCandidates.head
      whenever(headOut.additionalTokens.nonEmpty && headOut.additionalRegisters.nonEmpty) {

        val outputs = (0 until 10).map { i =>
          new ErgoBoxCandidate(headOut.value, headOut.ergoTree, i, headOut.additionalTokens, headOut.additionalRegisters)
        }
        val di = tx0.dataInputs
        val txIn = new ErgoLikeTransaction(tx0.inputs, di, tx0.outputCandidates ++ outputs)
        val tailOuts = txIn.outputCandidates.tail
        val headInput = txIn.inputs.head
        val tailInputs = txIn.inputs.tail
        val initialMessage = txIn.messageToSign

        // transaction with the same fields should have the same message to sign
        val otx2 = new ErgoLikeTransaction(headInput +: tailInputs, di, headOut +: tailOuts)
        (otx2.messageToSign sameElements initialMessage) shouldBe true

        /**
          * Check inputs modifications
          */
        // transaction with decreased number of inputs
        val itx3 = new ErgoLikeTransaction(tailInputs, di, txIn.outputCandidates)
        (itx3.messageToSign sameElements initialMessage) shouldBe false

        // transaction with increased number of inputs
        val itx4 = new ErgoLikeTransaction(headInput +: txIn.inputs, di, txIn.outputCandidates)
        (itx4.messageToSign sameElements initialMessage) shouldBe false

        // transaction with shuffled inputs
        if (tailInputs.nonEmpty) {
          val itx5 = new ErgoLikeTransaction(tailInputs ++ Seq(headInput), di, txIn.outputCandidates)
          (itx5.messageToSign sameElements initialMessage) shouldBe false
        }

        // transaction with modified input boxId
        val headInput6 = headInput.copy(boxId = txIn.outputs.head.id)
        val itx6 = new ErgoLikeTransaction(headInput6 +: tailInputs, di, txIn.outputCandidates)
        (itx6.messageToSign sameElements initialMessage) shouldBe false

        // transaction with modified input extension
        val newExtension7 = ContextExtension(headInput.spendingProof.extension.values ++ Map(Byte.MinValue -> ByteArrayConstant(Random.randomBytes(32))))
        val newProof7 = new ProverResult(headInput.spendingProof.proof, newExtension7)
        val headInput7 = headInput.copy(spendingProof = newProof7)
        val itx7 = new ErgoLikeTransaction(headInput7 +: tailInputs, di, txIn.outputCandidates)
        (itx7.messageToSign sameElements initialMessage) shouldBe false

        // transaction with modified input proof should not affect messageToSign
        val newProof8 = new ProverResult(headInput.spendingProof.proof, headInput.spendingProof.extension)
        val headInput8 = headInput.copy(spendingProof = newProof8)
        val itx8 = new ErgoLikeTransaction(headInput8 +: tailInputs, di, txIn.outputCandidates)
        (itx8.messageToSign sameElements initialMessage) shouldBe true

        /**
          * Check inputs modifications
          */
        // transaction with decreased number of data inputs
        if (di.nonEmpty) {
          val dtx3 = new ErgoLikeTransaction(txIn.inputs, di.tail, txIn.outputCandidates)
          (dtx3.messageToSign sameElements initialMessage) shouldBe false
        }

        // transaction with increased number of data inputs
        val di4 = DataInput(txIn.outputs.head.id)
        val dtx4 = new ErgoLikeTransaction(txIn.inputs, di4 +: di, txIn.outputCandidates)
        (dtx4.messageToSign sameElements initialMessage) shouldBe false

        // transaction with shuffled data inputs
        if (di.size > 1) {
          val dtx5 = new ErgoLikeTransaction(txIn.inputs, di.tail ++ Seq(di.head), txIn.outputCandidates)
          (dtx5.messageToSign sameElements initialMessage) shouldBe false
        }

        // transaction with modified data input boxId
        if (di.nonEmpty) {
          val di6 = DataInput(txIn.outputs.head.id)
          val dtx6 = new ErgoLikeTransaction(txIn.inputs, di6 +: di.tail, txIn.outputCandidates)
          (dtx6.messageToSign sameElements initialMessage) shouldBe false
        }

        /**
          * Check outputs modifications
          */
        // transaction with decreased number of outputs
        val otx3 = new ErgoLikeTransaction(txIn.inputs, di, tailOuts)
        (otx3.messageToSign sameElements initialMessage) shouldBe false

        // transaction with increased number of outputs
        val otx4 = new ErgoLikeTransaction(txIn.inputs, di, headOut +: txIn.outputCandidates)
        (otx4.messageToSign sameElements initialMessage) shouldBe false

        // transaction with shuffled outputs
        val otx5 = new ErgoLikeTransaction(txIn.inputs, di, tailOuts ++ Seq(txIn.outputCandidates.head))
        (otx5.messageToSign sameElements initialMessage) shouldBe false

        // transaction with modified output value
        val headOut6 = new ErgoBoxCandidate(headOut.value - 1, headOut.ergoTree, headOut.creationHeight, headOut.additionalTokens, headOut.additionalRegisters)
        val otx6 = new ErgoLikeTransaction(txIn.inputs, di, headOut6 +: tailOuts)
        (otx6.messageToSign sameElements initialMessage) shouldBe false

        // transaction with modified output tokens
        val newTokens = headOut.additionalTokens.map(t => t._1 -> (t._2 - 1))
        val headOut7 = new ErgoBoxCandidate(headOut.value, headOut.ergoTree, headOut.creationHeight, newTokens, headOut.additionalRegisters)
        val otx7 = new ErgoLikeTransaction(txIn.inputs, di, headOut7 +: tailOuts)
        (otx7.messageToSign sameElements initialMessage) shouldBe false

      }
    }
  }

}
