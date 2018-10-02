package sigmastate.utxo.examples

import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import org.ergoplatform._
import sigmastate.Values.ShortConstant
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter

class DemurrageExampleSpecification extends SigmaTestingCommons {
  /**
    * Demurrage currency example.
    *
    * The idea is that miners enforce users to combine a guarding script of a user ("regular_script") with a condition
    * that anyone (presumably, a miner) can spend no more "demurrage_cost" amount of tokens from an output of the user
    * after "demurrage_period" blocks since output creation. If the user is relocating the money from the output before
    * that height, the miner can charge according to output lifetime.
    *
    * We assume that it is enforced by a consensus protocol to store height when an input got into a block in the
    * register R3 (if the transaction is not included into the blockchain yet, then R3 contains the current height of
    * the blockchain).
    *
    * (regular_script) ∨
    * (height > (self.R3 + demurrage_period)
    *   ∧ has_output(value >= self.value − demurrage_cost, script = self.script, R3 + 50 <= height)
    * )
    */
  property("Evaluation - Demurrage Example") {
    val demurragePeriod = 100L
    val demurrageCost = 2

    //a blockchain node veryfing a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter()

    //backer's prover with his private key
    val userProver = new ErgoLikeTestProvingInterpreter().withContextExtender(15, ShortConstant(0))

    val regScript = userProver.dlogSecrets.head.publicImage

    val env = Map(
      "demurragePeriod" -> demurragePeriod,
      "demurrageCost" -> demurrageCost,
      "regScript" -> regScript
    )

    val prop = compile(env,
      """{
        | val outIdx = getVar[Short](15).get
        | val out = OUTPUTS(outIdx)
        |
        | val c2 = allOf(Array(
        |   HEIGHT >= SELF.R3[(Long, Array[Byte])].get._1 + demurragePeriod,
        |     out.value >= SELF.value - demurrageCost ,
        |     out.propositionBytes == SELF.propositionBytes,
        |     out.R3[(Long, Array[Byte])].get._1 <= HEIGHT,
        |     out.R3[(Long, Array[Byte])].get._1 >= HEIGHT - 50L
        | ))
        | regScript || c2
        | }
      """.stripMargin).asBoolValue

    /*
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val propTree = OR(
      regScript,
      AND(
        GE(Height, Plus(ExtractRegisterAs[STuple](Self, reg1).get.asTuple. , LongConstant(demurragePeriod))),
        Exists(Outputs, 21,
          AND(
            GE(ExtractAmount(TaggedBox(21)), Minus(ExtractAmount(Self), LongConstant(demurrageCost))),
            EQ(ExtractScriptBytes(TaggedBox(21)), ExtractScriptBytes(Self)),
            LE(ExtractRegisterAs[SLong.type](TaggedBox(21), reg1).get, Height),
            GE(ExtractRegisterAs[SLong.type](TaggedBox(21), reg1).get, Minus(Height, 50L))
          )
        )
      )
    )
    prop shouldBe propTree
    */

    val outHeight = 100
    val outValue = 10

    val ce = ContextExtension(Map(15.toByte -> ShortConstant(0)))

    //case 1: demurrage time hasn't come yet
    val currentHeight1 = outHeight + demurragePeriod - 1

    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(createBox(outValue, prop, currentHeight1)))

    val ctx1 = ErgoLikeContext(
      currentHeight1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = createBox(outValue, prop, outHeight),
      extension = ce)

    //user can spend all the money
    val uProof1 = userProver.prove(prop, ctx1, fakeMessage).get.proof
    verifier.verify(prop, ctx1, uProof1, fakeMessage).get._1 shouldBe true

    //miner can't spend any money
    verifier.verify(prop, ctx1, NoProof, fakeMessage).get._1 shouldBe false

    //case 2: demurrage time has come
    val currentHeight2 = outHeight + demurragePeriod
    val tx2 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(createBox(outValue, prop, currentHeight2)))
    val ctx2 = ErgoLikeContext(
      currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx2,
      self = createBox(outValue, prop, outHeight),
      extension = ce)

    //user can spend all the money
    val uProof2 = userProver.prove(prop, ctx2, fakeMessage).get.proof
    verifier.verify(prop, ctx2, uProof2, fakeMessage).get._1 shouldBe true

    //miner can spend "demurrageCost" tokens
    val currentHeight3 = outHeight + demurragePeriod
    val b = createBox(outValue - demurrageCost, prop, currentHeight3)
    val tx3 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(b))
    val ctx3 = ErgoLikeContext(
      currentHeight = currentHeight3,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(b),
      spendingTransaction = tx3,
      self = createBox(outValue, prop, outHeight),
      extension = ce)

    assert(ctx3.spendingTransaction.outputs.head.propositionBytes sameElements ctx3.self.propositionBytes)

    verifier.verify(prop, ctx3, NoProof, fakeMessage).get._1 shouldBe true

    //miner can't spend more
    val currentHeight4 = outHeight + demurragePeriod
    val tx4 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(createBox(outValue - demurrageCost - 1, prop, currentHeight4)))
    val ctx4 = ErgoLikeContext(
      currentHeight = currentHeight4,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx4,
      self = createBox(outValue, prop, outHeight),
      extension = ce)

    verifier.verify(prop, ctx4, NoProof, fakeMessage).get._1 shouldBe false

    //miner can spend less
    val tx5 = ErgoLikeTransaction(IndexedSeq(),
      IndexedSeq(createBox(outValue - demurrageCost + 1, prop, currentHeight4)))

    val ctx5 = ErgoLikeContext(
      currentHeight = currentHeight4,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx5,
      self = createBox(outValue, prop, outHeight),
      extension = ce)

    verifier.verify(prop, ctx5, NoProof, fakeMessage).get._1 shouldBe true
  }
}
