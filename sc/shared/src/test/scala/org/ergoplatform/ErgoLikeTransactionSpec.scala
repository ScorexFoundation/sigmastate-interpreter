package org.ergoplatform

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.settings.ErgoAlgos
import scorex.util.encode.Base16
import scorex.util.{ModifierId, Random}
import sigma.ast.SCollection.SByteArray
import sigma.ast.{SInt, SPair, SSigmaProp, SType}
import sigma.ast._
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.SigmaSerializer
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate._
import sigma.ast.SType._
import sigmastate.helpers.TestingHelpers.copyTransaction
import sigmastate.utils.Helpers
import sigma.SigmaDslTesting
import sigma.Extensions._
import sigma.ast.global.TrueSigmaProp
import sigma.data.{CSigmaProp, Digest32Coll, TrivialProp}

class ErgoLikeTransactionSpec extends SigmaDslTesting {

  property("ErgoBox test vectors") {
    val token1 = "6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001"
    val token2 = "a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600"
    val b1 = new ErgoBoxCandidate(
      10L,
      ErgoTree(ErgoTree.ZeroHeader, Vector(), TrueSigmaProp),
      100,
      Coll(
        (Digest32Coll @@@ (ErgoAlgos.decodeUnsafe(token1).toColl)) -> 10000000L,
        (Digest32Coll @@@ (ErgoAlgos.decodeUnsafe(token2).toColl)) -> 500L
      )
    )
    val b1_clone = new ErgoBoxCandidate(
      10L,
      ErgoTree(ErgoTree.ZeroHeader, Vector(), TrueSigmaProp),
      100,
      Coll(
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token1).toColl) -> 10000000L,
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token2).toColl) -> 500L
      )
    )
    val b2 = new ErgoBoxCandidate(
      10L,
      ErgoTree(ErgoTree.ConstantSegregationHeader, Vector(TrueSigmaProp), ConstantPlaceholder(0, SSigmaProp)),
      100,
      Coll(
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token1).toColl) -> 10000000L,
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token1).toColl) -> 500L,
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token2).toColl) -> 500L
      )
    )
    val b3 = new ErgoBox(
      10L,
      ErgoTree(ErgoTree.DefaultHeader, Vector(), TrueSigmaProp),
      Coll(
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token1).toColl) -> 10000000L,
        Digest32Coll @@ (ErgoAlgos.decodeUnsafe(token2).toColl) -> 500L
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
    b2.tokens shouldBe Map[ModifierId, Long](ModifierId @@ token1 -> 10000500L, ModifierId @@ token2 -> 500L)

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
        (Digest32Coll @@ ErgoAlgos.decodeUnsafe(token1).toColl) -> 10000000L,
        (Digest32Coll @@ ErgoAlgos.decodeUnsafe(token2).toColl) -> 500L
      ).map(identity).toConstant
      // TODO v6.0 (16h): fix collections equality and remove map(identity)
      //  (PairOfColl should be equal CollOverArray but now it is not)
      // see (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/909)
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
        val tokens = out.additionalTokens.map(v => (v._1.toArray.clone().toColl.asInstanceOf[TokenId], v._2))
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
          bytes.indexOfSlice(tokenId.toArray) should not be -1
          bytes.indexOfSlice(tokenId.toArray) shouldBe bytes.lastIndexOfSlice(tokenId.toArray)
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
    forAll { (tx: ErgoLikeTransaction, startIndex: Byte, endIndex: Byte) =>
      whenever(endIndex >= startIndex) {
        val idRange = endIndex - startIndex

        val ce = ContextExtension(startIndex.to(endIndex).map(id => id.toByte -> IntConstant(4)).toMap)
        val wrongInput = Input(tx.inputs.head.boxId, ProverResult(Array.emptyByteArray, ce))
        val ins = IndexedSeq(wrongInput) ++ tx.inputs.tail
        val tx2 = copyTransaction(tx)(inputs = ins)

        def roundtrip() = {
          val bs = ErgoLikeTransactionSerializer.toBytes(tx2)
          val restored = ErgoLikeTransactionSerializer.parse(
            SigmaSerializer.startReader(bs, 0)
          )
          restored.inputs.head.extension.values.size shouldBe tx2.inputs.head.extension.values.size
        }

        if(idRange < 127) {
          roundtrip()
        } else {
          assertExceptionThrown(
            roundtrip(),
            { _ => true }
          )
        }
      }
    }
  }

  property("Tuple in register test vector") {
    val txId = "b5fd96c9f8c1ff436d609a225a12377c6873b890a145fc05f4099845b89315e5"
    val txHex = "0278c93ee55eec066ec06290524dc01773440fbaff644bd181ab4334f97083863738644bf834de150b9abd077cf52ecb4892536ab6ad6ef8505fb932d9ca7b9fb211fc8294618723eb4909d4553f4d419762332f68865b1d703d00faae81c0e57683108c55647c5837a4fedfb52ffde04db74a8b4b77e37ab81afc3828c904862db6134e0f859ff5f62e4a5cb9da6b15ea1cd1a11dc932899d7b99e9073034ccd4e7c54fa9790d8ee356dce22ee151b044561c96000000038094ebdc031007040205c80105c8010500040004000e1f3faf2cb329f2e90d6d23b58d91bbb6c046aa143261cc21f52fbe2824bfcbf0d807d601e4c6a70408d602b2a5730000d603e4c6a70601d604e4c6a70856d605e4c6a70505d606e4c6a70705d60795720399c1a7c1720299c17202c1a7eb027201d1ededededededededed93c27202c2a793e4c672020408720193e4c6720205059572039d9c72057e8c7204010573019d9c72057e8c72040205730294e4c672020601720393e4c672020705720693e4c672020856720493e4c67202090ec5a7929c720672057207917207730395ef720393b1db630872027304d801d608b2db63087202730500ed938c7208017306938c7208027206d18f34000508cd030c8f9c4dc08f3c006fa85a47c9156dedbede000a8b764c6e374fd097e873ba0405dceeaa0401010564860202660263c0edcea7090008cd030c8f9c4dc08f3c006fa85a47c9156dedbede000a8b764c6e374fd097e873ba04d18f340000c0843d1005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304d18f340000"
    val txBytes = Base16.decode(txHex).get
    val tx = ErgoLikeTransactionSerializer.fromBytes(txBytes)
    tx.id shouldBe txId
  }
}
