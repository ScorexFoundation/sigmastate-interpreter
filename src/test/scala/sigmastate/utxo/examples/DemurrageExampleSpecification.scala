package sigmastate.utxo.examples

import sigmastate.Values.{LongConstant, TaggedBox, SigmaPropConstant}
import sigmastate._
import sigmastate.interpreter.Interpreter._
import org.ergoplatform._
import sigmastate.Values.ShortConstant
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter

class DemurrageExampleSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

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
    val demurrageCoeff = 2

    val outIdxVarId = Byte.MaxValue

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter()

    //backer's prover with his private key
    val userProver = new ErgoLikeTestProvingInterpreter().withContextExtender(outIdxVarId, ShortConstant(0))

    val minerProver = new ErgoLikeTestProvingInterpreter().withContextExtender(outIdxVarId, ShortConstant(0))

    val regScript = userProver.dlogSecrets.head.publicImage

    val env = Map(
      ScriptNameProp -> "Demurrage",
      "demurragePeriod" -> demurragePeriod,
      "demurrageCoeff" -> demurrageCoeff,
      "regScript" -> regScript
    )

    //todo: add condition on
    val prop = compile(env,
      """{
        | val outIdx = getVar[Short](127).get
        | val out = OUTPUTS(outIdx)
        |
        | val c1 = allOf(Col(
        |   HEIGHT >= SELF.R3[(Long, Col[Byte])].get._1 + demurragePeriod,
        |   SELF.value - demurrageCoeff * SELF.bytes.size * (HEIGHT - SELF.R3[(Long, Col[Byte])].get._1) <= 0
        | ))
        |
        | val c2 = allOf(Col(
        |   HEIGHT >= SELF.R3[(Long, Col[Byte])].get._1 + demurragePeriod,
        |   out.R3[(Long, Col[Byte])].get._1 == HEIGHT,
        |   out.value >= SELF.value - demurrageCoeff * SELF.bytes.size * (HEIGHT - SELF.R3[(Long, Col[Byte])].get._1),
        |   out.propositionBytes == SELF.propositionBytes
        | ))
        |
        | anyOf(Col(regScript, c1, c2))
        | }
      """.stripMargin).asBoolValue

    /*
    todo: fix / uncomment
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val propTree = BinOr(
      SigmaPropConstant(regScript).isValid,
      AND(
        GE(Height, Plus(ExtractRegisterAs[STuple](Self, reg1).get.asTuple. , LongConstant(demurragePeriod))),
        Exists(Outputs, 21,
          BinAnd(
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

    val inHeight = 0
    val outValue = 100
    val approxSize = createBox(outValue, prop, inHeight).bytes.length + 2
    val inValue: Int = (outValue + demurrageCoeff * demurragePeriod * approxSize).toInt

    val ce = ContextExtension(Map(outIdxVarId -> ShortConstant(0)))

    //case 1: demurrage time hasn't come yet
    val currentHeight1 = inHeight + demurragePeriod - 1

    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(createBox(outValue, prop, currentHeight1)))

    val ctx1 = ErgoLikeContext(
      currentHeight1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = createBox(inValue, prop, inHeight),
      extension = ce)

    //user can spend all the money
    val uProof1 = userProver.prove(prop, ctx1, fakeMessage).get.proof
    verifier.verify(emptyEnv, prop, ctx1, uProof1, fakeMessage).get._1 shouldBe true

    //miner can't spend any money
    minerProver.prove(prop, ctx1, fakeMessage).isSuccess shouldBe false
    verifier.verify(prop, ctx1, NoProof, fakeMessage).get._1 shouldBe false

    //case 2: demurrage time has come
    val currentHeight2 = inHeight + demurragePeriod
    val tx2 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(createBox(outValue, prop, currentHeight2)))
    val ctx2 = ErgoLikeContext(
      currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx2,
      self = createBox(inValue, prop, inHeight),
      extension = ce)

    //user can spend all the money
    val uProof2 = userProver.prove(prop, ctx2, fakeMessage).get.proof
    verifier.verify(env, prop, ctx2, uProof2, fakeMessage).get._1 shouldBe true

    //miner can spend "demurrageCoeff * demurragePeriod" tokens
    val b = createBox(outValue, prop, currentHeight2)
    val tx3 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(b))
    val ctx3 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(b),
      spendingTransaction = tx3,
      self = createBox(inValue, prop, inHeight))

    assert(ctx3.spendingTransaction.outputs.head.propositionBytes sameElements ctx3.self.propositionBytes)

    val mProverRes1 = minerProver.prove(prop, ctx3, fakeMessage).get
    val _ctx3: ErgoLikeContext = ctx3.withExtension(mProverRes1.extension)
    verifier.verify(prop, _ctx3, mProverRes1, fakeMessage: Array[Byte]).get._1 shouldBe true

    //miner can't spend more
    val b2 = createBox(outValue - 1, prop, currentHeight2)
    val tx4 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(b2))
    val ctx4 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(b2),
      spendingTransaction = tx4,
      self = createBox(inValue, prop, inHeight),
      extension = ce)

    minerProver.prove(prop, ctx4, fakeMessage).isSuccess shouldBe false
    verifier.verify(prop, ctx4, NoProof, fakeMessage).get._1 shouldBe false

    //miner can spend less
    val tx5 = ErgoLikeTransaction(IndexedSeq(),
      IndexedSeq(createBox(outValue + 1, prop, currentHeight2)))

    val ctx5 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx5,
      self = createBox(inValue, prop, inHeight),
      extension = ce)

    val mProof2 = minerProver.prove(prop, ctx5, fakeMessage).get
    verifier.verify(prop, ctx5, mProof2, fakeMessage).get._1 shouldBe true

    //miner can destroy a box if it contains less than the storage fee
    val iv = inValue - outValue
    val b3 = createBox(iv, Values.FalseLeaf, currentHeight2)
    val tx6 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(b3))

    val ctx6 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(b3),
      spendingTransaction = tx6,
      self = createBox(iv, prop, inHeight),
      extension = ce)

    val mProof3 = minerProver.prove(prop, ctx6, fakeMessage).get
    verifier.verify(prop, ctx6, mProof3, fakeMessage).get._1 shouldBe true

  }
}
