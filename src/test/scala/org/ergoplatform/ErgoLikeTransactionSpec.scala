package org.ergoplatform

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.helpers.SigmaTestingCommons
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
          new ErgoBoxCandidate(out.value, out.proposition, i, out.additionalTokens, out.additionalRegisters)
        }
        val tx = ErgoLikeTransaction(txIn.inputs, txIn.outputCandidates ++ outputs)
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


}
