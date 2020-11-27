package org.ergoplatform

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.settings.ErgoAlgos
import scalan.Nullable
import scorex.crypto.hash.Digest32
import scorex.util.{Random, ModifierId}
import sigmastate.SCollection.SByteArray
import sigmastate.{SSigmaProp, SPair, SInt, TrivialProp, SType}
import sigmastate.Values.{LongConstant, FalseLeaf, ConstantNode, SigmaPropConstant, ConstantPlaceholder, TrueSigmaProp, ByteArrayConstant, IntConstant, ErgoTree}
import sigmastate.interpreter.{ProverResult, ContextExtension, VersionContext}
import sigmastate.serialization.SigmaSerializer
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.SType._
import sigmastate.helpers.TestingHelpers.copyTransaction
import sigmastate.utils.Helpers
import special.sigma.SigmaDslTesting

class ErgoLikeTransactionSpec extends SigmaDslTesting {

  property("ErgoBox test vectors") {
    val token1 = "6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001"
    val token2 = "a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600"
    val b1 = new ErgoBoxCandidate(
      10L,
      ErgoTree(ErgoTree.DefaultHeader, Vector(), TrueSigmaProp),
      100,
      Coll(
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token1)) -> 10000000L,
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token2)) -> 500L
      )
    )
    val b1_clone = new ErgoBoxCandidate(
      10L,
      ErgoTree(ErgoTree.DefaultHeader, Vector(), TrueSigmaProp),
      100,
      Coll(
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token1)) -> 10000000L,
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token2)) -> 500L
      )
    )
    val b2 = new ErgoBoxCandidate(
      10L,
      ErgoTree(ErgoTree.ConstantSegregationHeader, Vector(TrueSigmaProp), ConstantPlaceholder(0, SSigmaProp)),
      100,
      Coll(
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token1)) -> 10000000L,
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token2)) -> 500L
      )
    )
    val b3 = new ErgoBox(
      10L,
      ErgoTree(ErgoTree.DefaultHeader, Vector(), TrueSigmaProp),
      Coll(
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token1)) -> 10000000L,
        Digest32 @@ (ErgoAlgos.decodeUnsafe(token2)) -> 500L
      ),
      Map(
        ErgoBox.R5 -> ByteArrayConstant(Helpers.decodeBytes("7fc87f7f01ff")),
        ErgoBox.R4 -> FalseLeaf
      ),
      ModifierId @@ ("218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080"),
      0.toShort,
      100
    )
    val expectedTokens = Map[ModifierId, Long](ModifierId @@ token1 -> 10000000L, ModifierId @@ token2 -> 500L)
    val assetHolder = ErgoBoxAssetsHolder(10L, expectedTokens)

    b1.tokens shouldBe expectedTokens
    b1_clone.tokens shouldBe expectedTokens
    b3.tokens shouldBe expectedTokens

    assertResult(true)(b1.hashCode() == b1.hashCode())
    assertResult(true)(b1 == b1)
    assertResult(true)(b1 != assetHolder)

    assertResult(true)(b1.hashCode() == b1_clone.hashCode())
    assertResult(true)(b1 == b1_clone)

    assertResult(true)(b1.hashCode() != b3.hashCode() || b1 != b3)
    assertResult(true)(b1 != b3)
    assertResult(true)(b3 != b1)

    val b4 = b3.toCandidate
    assertResult(true)(b3 != b4)
    assertResult(true)(b4 == b3) // asymmetic !!!
    assertResult(true)(b3.hashCode() != b4.hashCode())

    b1.get(ErgoBox.R0).get shouldBe LongConstant(10)
    b1.get(ErgoBox.R1).get shouldBe ByteArrayConstant(Helpers.decodeBytes("0008d3"))

    { // test case for R2
      val res = b1.get(ErgoBox.R2).get
      val exp = Coll(
        ErgoAlgos.decodeUnsafe(token1).toColl -> 10000000L,
        ErgoAlgos.decodeUnsafe(token2).toColl -> 500L
      ).map(identity).toConstant
      // TODO HF (16h): fix collections equality and remove map(identity)
      //  (PairOfColl should be equal CollOverArray but now it is not)
      res shouldBe exp
    }

    { // test case for R3
      val res = b1.get(ErgoBox.R3).get
      res shouldBe ConstantNode[SType](
        (100, Helpers.decodeBytes("00000000000000000000000000000000000000000000000000000000000000000000")).asWrappedType,
        SPair(SInt, SByteArray)
      )
    }

    { // test case for R5
      val res = b3.get(ErgoBox.R5).get
      res shouldBe ByteArrayConstant(Helpers.decodeBytes("7fc87f7f01ff"))
    }

    { // for proposition
      b1.proposition shouldBe SigmaPropConstant(CSigmaProp(TrivialProp.TrueProp))
      b2.proposition shouldBe SigmaPropConstant(CSigmaProp(TrivialProp.TrueProp))
    }
  }

  property("ErgoLikeTransaction: Serializer round trip") {
    forAll(MinSuccessful(50)) { t: ErgoLikeTransaction => roundTripTest(t)(ErgoLikeTransaction.serializer) }
    forAll(MinSuccessful(50)) { t: ErgoLikeTransaction => roundTripTestWithPos(t)(ErgoLikeTransaction.serializer) }
  }

  property("ErgoLikeTransaction with same token in different outputs : Serializer round trip") {
    forAll { txIn: ErgoLikeTransaction =>
      whenever(txIn.outputCandidates.head.additionalTokens.nonEmpty) {
        val out = txIn.outputCandidates.head
        // clone tokenIds so that same id have different references
        val tokens = out.additionalTokens.map(v => (v._1.clone().asInstanceOf[TokenId], v._2))
        val outputs = (0 until 10).map { i =>
          new ErgoBoxCandidate(out.value, out.ergoTree, i, tokens, out.additionalRegisters)
        }
        val tx = new ErgoLikeTransaction(txIn.inputs, txIn.dataInputs, txIn.outputCandidates ++ outputs)
        roundTripTestWithPos(tx)(ErgoLikeTransaction.serializer)

        // check that token id is written only once
        val w = SigmaSerializer.startWriter()
        ErgoLikeTransaction.serializer.serialize(tx, w)
        val bytes = w.toBytes

        tx.outputCandidates.toColl.flatMap(_.additionalTokens).foreach { (tokenId, _) =>
          bytes.indexOfSlice(tokenId) should not be -1
          bytes.indexOfSlice(tokenId) shouldBe bytes.lastIndexOfSlice(tokenId)
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

        // ProverResult equality checks
        val copiedResult = new ProverResult(newProof7.proof, newProof7.extension)
        assertResult(true)(copiedResult == newProof7)
        assertResult(true, "hash codes")(copiedResult.hashCode() == newProof7.hashCode())
        assertResult(true, "different types")(newProof7 != headInput7)
        assertResult(true, "different extensions")(newProof8 != newProof7)
        val tmpResult = new ProverResult(Array[Byte](), newProof7.extension)
        assertResult(true, "different proofs")(tmpResult != newProof7)
        assertResult(true)(newProof8.hashCode() != newProof7.hashCode() || newProof8 != newProof7)

        /**
          * Check data inputs modifications
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

  property("context extension serialization") {
    forAll { tx: ErgoLikeTransaction =>
      val idRange = Byte.MinValue to (Byte.MaxValue - 1)
      val ce = ContextExtension(idRange.map(id => id.toByte -> IntConstant(4)).toMap)
      val wrongInput = Input(tx.inputs.head.boxId, ProverResult(Array.emptyByteArray, ce))
      val ins = IndexedSeq(wrongInput) ++ tx.inputs.tail
      val tx2 = copyTransaction(tx)(inputs = ins)
      def roundtrip(version: Nullable[VersionContext]) = {
        val bs = ErgoLikeTransactionSerializer.toBytes(tx2)
        val restored = ErgoLikeTransactionSerializer.parse(
          SigmaSerializer.startReader(bs, 0, version)
        )
        restored.inputs.head.extension.values.size shouldBe tx2.inputs.head.extension.values.size
      }

      // successful for v4.0 and above
      roundtrip(Nullable(VersionContext.MaxSupportedVersion))

      // unsuccessful if version context is not passed (which means v3.x version)
      assertExceptionThrown(
        roundtrip(Nullable.None),
        { _ => true }
      )
    }
  }
}
