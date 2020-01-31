package sigmastate.utxo.examples

import org.ergoplatform._
import scorex.util.ScorexLogging
import sigmastate.Values.IntConstant
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.lang.Terms._
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons._
import sigmastate.utxo._
import sigmastate._
import sigmastate.eval._

/**
  * An example of currency emission contract.
  * Instead of having implicit emission via coinbase transaction, we put 1 coin into genesis state with a script
  * that controls emission.
  * This script is corresponding to the whitepaper. Please note that Ergo has different contract
  * defined in ErgoScriptPredef.
  */
class CoinEmissionSpecification extends SigmaTestingCommons with ScorexLogging {
  // don't use TestingIRContext, this suite also serves the purpose of testing the RuntimeIRContext
  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    // uncomment if you want to log script evaluation
    // override val okPrintEvaluatedEntries = true
    saveGraphsInFile = false
    outputEstimatedCost = false
    outputComputedResults = false
  }

  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private val coinsInOneErgo: Long = 100000000
  private val blocksPerHour: Int = 30

  case class MonetarySettings(fixedRatePeriod: Int,
                              epochLength: Int,
                              fixedRate: Long,
                              oneEpochReduction: Long)

  val s = MonetarySettings(blocksPerHour * 24 * 7, 24 * blocksPerHour, 15 * coinsInOneErgo, 3 * coinsInOneErgo)

  val (coinsTotal, blocksTotal) = {
    def loop(height: Int, acc: Long): (Long, Int) = {
      val currentRate = emissionAtHeight(height)
      if (currentRate > 0) {
        loop(height + 1, acc + currentRate)
      } else {
        (acc, height - 1)
      }
    }

    loop(0, 0)
  }

  def emissionAtHeight(h: Int): Long = {
    if (h < s.fixedRatePeriod) {
      s.fixedRate
    } else {
      val epoch = 1 + (h - s.fixedRatePeriod) / s.epochLength
      Math.max(s.fixedRate - s.oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  /*
block 0 in 0 ms, 2430000000000 coins remain, defs: 0
block 0 in 41 ms, 2430000000000 coins remain
block 100 in 1292 ms, 2280000000000 coins remain, defs: 61661
block 200 in 965 ms, 2130000000000 coins remain, defs: 61661
block 300 in 991 ms, 1980000000000 coins remain, defs: 61661
block 400 in 842 ms, 1830000000000 coins remain, defs: 61661
block 500 in 833 ms, 1680000000000 coins remain, defs: 61661
block 600 in 788 ms, 1530000000000 coins remain, defs: 61661
block 700 in 903 ms, 1380000000000 coins remain, defs: 61661
block 800 in 789 ms, 1230000000000 coins remain, defs: 61661
block 900 in 774 ms, 1080000000000 coins remain, defs: 61661
block 1000 in 753 ms, 930000000000 coins remain, defs: 61661
block 1000 in 8889 ms, 930000000000 coins remain
block 1100 in 764 ms, 780000000000 coins remain, defs: 61661
block 1200 in 886 ms, 630000000000 coins remain, defs: 61661
block 1300 in 1371 ms, 480000000000 coins remain, defs: 61661
block 1400 in 1908 ms, 330000000000 coins remain, defs: 61661
block 1500 in 1626 ms, 180000000000 coins remain, defs: 61661
block 1600 in 1622 ms, 30000000000 coins remain, defs: 61661
  */
  property("emission specification") {
    val register = reg1
    val prover = new ContextEnrichingTestProvingInterpreter()

    val rewardOut = ByIndex(Outputs, IntConstant(0))

    val epoch =
      Upcast(
        Plus(IntConstant(1), Divide(Minus(Height, IntConstant(s.fixedRatePeriod)), IntConstant(s.epochLength))),
        SLong)

    val coinsToIssue = If(LT(Height, IntConstant(s.fixedRatePeriod)),
      s.fixedRate,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(ExtractRegisterAs[SInt.type](rewardOut, register).get, Height)
    val heightIncreased = GT(Height, ExtractRegisterAs[SInt.type](Self, register).get)
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)

    val prop = BinOr(
      AND(heightCorrect, heightIncreased, sameScriptRule, correctCoinsConsumed),
      BinAnd(heightIncreased, lastCoins)
    ).toSigmaProp

    val env = Map("fixedRatePeriod" -> s.fixedRatePeriod,
      "epochLength" -> s.epochLength,
      "fixedRate" -> s.fixedRate,
      "oneEpochReduction" -> s.oneEpochReduction)

    val prop1 = compileWithoutCosting(env,
      """{
        |    val epoch = 1 + ((HEIGHT - fixedRatePeriod) / epochLength)
        |    val out = OUTPUTS(0)
        |    val coinsToIssue = if(HEIGHT < fixedRatePeriod) fixedRate else fixedRate - (oneEpochReduction * epoch)
        |    val correctCoinsConsumed = coinsToIssue == (SELF.value - out.value)
        |    val sameScriptRule = SELF.propositionBytes == out.propositionBytes
        |    val heightIncreased = HEIGHT > SELF.R4[Int].get
        |    val heightCorrect = out.R4[Int].get == HEIGHT
        |    val lastCoins = SELF.value <= oneEpochReduction
        |    sigmaProp(allOf(Coll(heightCorrect, heightIncreased, sameScriptRule, correctCoinsConsumed)) || (heightIncreased && lastCoins))
        |}""".stripMargin).asSigmaProp

    prop1 shouldEqual prop

    val minerImage = prover.dlogSecrets.head.publicImage
    val minerPubkey = minerImage.pkBytes
    val minerProp = minerImage

    val initialBoxCandidate: ErgoBox = ErgoBox(coinsTotal / 4, prop, 0, Seq(), Map(register -> IntConstant(-1)))
    val initBlock = FullBlock(IndexedSeq(createTransaction(initialBoxCandidate)), minerPubkey)
    val genesisState = ValidationState.initialState(initBlock)
    val fromState = genesisState.boxesReader.byId(genesisState.boxesReader.allIds.head).get
    val initialBox = new ErgoBox(initialBoxCandidate.value, initialBoxCandidate.ergoTree,
      initialBoxCandidate.additionalTokens, initialBoxCandidate.additionalRegisters,
      initBlock.txs.head.id, 0, 0)
    initialBox shouldBe fromState

    def genCoinbaseLikeTransaction(state: ValidationState,
                                   emissionBox: ErgoBox,
                                   height: Int): ErgoLikeTransaction = {
      assert(state.state.currentHeight == height - 1)
      val ut = if (emissionBox.value > s.oneEpochReduction) {
        val minerBox = new ErgoBoxCandidate(emissionAtHeight(height), minerProp, height, Colls.emptyColl, Map())
        val newEmissionBox: ErgoBoxCandidate =
          new ErgoBoxCandidate(emissionBox.value - minerBox.value, prop, height, Colls.emptyColl, Map(register -> IntConstant(height)))

        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(newEmissionBox, minerBox)
        )
      } else {
        val minerBox1 = new ErgoBoxCandidate(emissionBox.value - 1, minerProp, height, Colls.emptyColl, Map(register -> IntConstant(height)))
        val minerBox2 = new ErgoBoxCandidate(1, minerProp, height, Colls.emptyColl, Map(register -> IntConstant(height)))
        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(minerBox1, minerBox2)
        )
      }

      val context = ErgoLikeContextTesting(height,
        state.state.lastBlockUtxoRoot,
        minerPubkey,
        IndexedSeq(emissionBox),
        ut,
        emissionBox,
        ContextExtension.empty)
      val proverResult = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, context, ut.messageToSign).get
      ut.toSigned(IndexedSeq(proverResult))
    }

    var st = System.currentTimeMillis()
    var thousandTime = System.currentTimeMillis()

    def chainGen(state: ValidationState,
                 emissionBox: ErgoBox,
                 height: Int,
                 hLimit: Int): Unit = if (height < hLimit) {
      if (height % 100 == 0) {
        val t = System.currentTimeMillis()
        println(s"block $height in ${t - st} ms, ${emissionBox.value} coins remain, defs: ${IR.defCount}")
        st = t
        IR.resetContext()
      }
      if (height % 1000 == 0) {
        val t = System.currentTimeMillis()
        println(s"block $height in ${t - thousandTime} ms, ${emissionBox.value} coins remain")
        thousandTime = t
      }
      val tx = genCoinbaseLikeTransaction(state, emissionBox, height)
      val block = FullBlock(IndexedSeq(tx), minerPubkey)
      val newState = state.applyBlock(block).get
      if (tx.outputs.last.value > 1) {
        val newEmissionBox = newState.boxesReader.byId(tx.outputs.head.id).get
        chainGen(newState, newEmissionBox, height + 1, hLimit)
      } else {
        log.debug(s"Emission box is consumed at height $height")
      }
    }

    chainGen(genesisState, initialBox, 0, 1000000)
  }
}
