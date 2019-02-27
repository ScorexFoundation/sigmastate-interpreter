package sigmastate.utxo.examples

import sigmastate.Values.{LongConstant, SigmaPropConstant, TaggedBox}
import sigmastate._
import sigmastate.interpreter.Interpreter._
import org.ergoplatform._
import sigmastate.Values.ShortConstant
import sigmastate._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.Terms._

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
    val demurragePeriod = 100
    val demurrageCoeff = 2

    val outIdxVarId = Byte.MaxValue

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter()

    //backer's prover with his private key
    val userProver = new ContextEnrichingTestProvingInterpreter().withContextExtender(outIdxVarId, ShortConstant(0))

    val minerProver = new ContextEnrichingTestProvingInterpreter().withContextExtender(outIdxVarId, ShortConstant(0))

    val regScript = userProver.dlogSecrets.head.publicImage

    val env = Map(
      ScriptNameProp -> "Demurrage",
      "demurragePeriod" -> demurragePeriod,
      "demurrageCoeff" -> demurrageCoeff,
      "regScript" -> regScript
    )

    //todo: add condition on
    val prop = compileWithCosting(env,
      """{
        | val outIdx = getVar[Short](127).get
        | val out = OUTPUTS(outIdx)
        |
        | val c1 = allOf(Coll(
        |   HEIGHT >= SELF.R3[(Int, Coll[Byte])].get._1 + demurragePeriod,
        |   SELF.value - demurrageCoeff * SELF.bytes.size * (HEIGHT - SELF.R3[(Int, Coll[Byte])].get._1) <= 0
        | ))
        |
        | val c2 = allOf(Coll(
        |   HEIGHT >= SELF.R3[(Int, Coll[Byte])].get._1 + demurragePeriod,
        |   out.R3[(Int, Coll[Byte])].get._1 == HEIGHT,
        |   out.value >= SELF.value - demurrageCoeff * SELF.bytes.size * (HEIGHT - SELF.R3[(Int, Coll[Byte])].get._1),
        |   out.propositionBytes == SELF.propositionBytes
        | ))
        |
        | anyOf(Coll(regScript, c1, c2))
        | }
      """.stripMargin).asSigmaProp

    /*
    todo: fix / uncomment
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val propTree = BinOr(
      SigmaPropConstant(regScript).isProven,
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

    val tx1 = createTransaction(createBox(outValue, prop, currentHeight1))
    val selfBox = createBox(inValue, prop, inHeight)
    val ctx1 = ErgoLikeContext(
      currentHeight1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx1,
      self = selfBox,
      extension = ce)

    //user can spend all the money
    val uProof1 = userProver.prove(prop, ctx1, fakeMessage).get.proof
    verifier.verify(emptyEnv, prop, ctx1, uProof1, fakeMessage).get._1 shouldBe true

    //miner can't spend any money
    minerProver.prove(prop, ctx1, fakeMessage).isSuccess shouldBe false
    verifier.verify(prop, ctx1, NoProof, fakeMessage).get._1 shouldBe false

    //case 2: demurrage time has come
    val currentHeight2 = inHeight + demurragePeriod
    val tx2 = createTransaction(createBox(outValue, prop, currentHeight2))
    val ctx2 = ErgoLikeContext(
      currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx2,
      self = selfBox,
      extension = ce)

    //user can spend all the money
    val uProof2 = userProver.prove(prop, ctx2, fakeMessage).get.proof
    verifier.verify(env, prop, ctx2, uProof2, fakeMessage).get._1 shouldBe true

    //miner can spend "demurrageCoeff * demurragePeriod" tokens
    val b = createBox(outValue, prop, currentHeight2)
    val tx3 = createTransaction(b)
    val ctx3 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(b, selfBox),
      spendingTransaction = tx3,
      self = selfBox)

    assert(ctx3.spendingTransaction.outputs.head.propositionBytes sameElements ctx3.self.propositionBytes)

    val mProverRes1 = minerProver.prove(prop, ctx3, fakeMessage).get
    val _ctx3: ErgoLikeContext = ctx3.withExtension(mProverRes1.extension)
    verifier.verify(prop, _ctx3, mProverRes1, fakeMessage: Array[Byte]).get._1 shouldBe true

    //miner can't spend more
    val b2 = createBox(outValue - 1, prop, currentHeight2)
    val tx4 = createTransaction(b2)
    val ctx4 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(b2, selfBox),
      spendingTransaction = tx4,
      self = selfBox,
      extension = ce)

    minerProver.prove(prop, ctx4, fakeMessage).isSuccess shouldBe false
    verifier.verify(prop, ctx4, NoProof, fakeMessage).get._1 shouldBe false

    //miner can spend less
    val tx5 = createTransaction(createBox(outValue + 1, prop, currentHeight2))

    val ctx5 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx5,
      self = selfBox,
      extension = ce)

    val mProof2 = minerProver.prove(prop, ctx5, fakeMessage).get
    verifier.verify(prop, ctx5, mProof2, fakeMessage).get._1 shouldBe true

    //miner can destroy a box if it contains less than the storage fee
    val iv = inValue - outValue
    val b3 = createBox(iv, ErgoScriptPredef.FalseProp, currentHeight2)
    val tx6 = createTransaction(b3)
    val selfBox6 = createBox(iv, prop, inHeight)
    val ctx6 = ErgoLikeContext(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(b3, selfBox6),
      spendingTransaction = tx6,
      self = selfBox6,
      extension = ce)

    val mProof3 = minerProver.prove(prop, ctx6, fakeMessage).get
    verifier.verify(prop, ctx6, mProof3, fakeMessage).get._1 shouldBe true

  }
}
