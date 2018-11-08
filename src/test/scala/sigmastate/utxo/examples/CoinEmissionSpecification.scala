package sigmastate.utxo.examples

import org.ergoplatform.{ErgoLikeContext, Height, _}
import scorex.util.ScorexLogging
import sigmastate.Values.{LongConstant, IntConstant, SValue}
import sigmastate.Values.{ConcreteCollection, IntConstant, LongConstant}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv, ScriptEnv}
import sigmastate.serialization.OpCodes
import sigmastate.utxo.BlockchainSimulationSpecification.{ValidationState, Block}
import sigmastate.utxo._
import sigmastate.{SLong, _}
import sigmastate.lang.Terms._

/**
  * Coin emission specification.
  * Instead of having implicit emission via coinbase transaction, we implement 1 output in a state with script,
  * that controls emission rules
  */
class CoinEmissionSpecification extends SigmaTestingCommons with ScorexLogging {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
    // overrided to avoid outputing intermediate graphs in files (too many of them)
    override def onCostingResult[T](env: ScriptEnv, tree: SValue, res: CostingResult[T]): Unit = {
    }
  }

  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private val coinsInOneErgo: Long = 100000000
  private val blocksPerHour: Int = 30

  case class MonetarySettings(fixedRatePeriod: Long,
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

  def emissionAtHeight(h: Long): Long = {
    if (h < s.fixedRatePeriod) {
      s.fixedRate
    } else {
      val epoch = 1 + (h - s.fixedRatePeriod) / s.epochLength
      Math.max(s.fixedRate - s.oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  ignore("emission specification") {
    val register = reg1
    val prover = new ErgoLikeTestProvingInterpreter()

    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val epoch = Plus(LongConstant(1), Divide(Minus(Height, LongConstant(s.fixedRatePeriod)), LongConstant(s.epochLength)))
    val coinsToIssue = If(LT(Height, LongConstant(s.fixedRatePeriod)),
      s.fixedRate,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(ExtractRegisterAs[SLong.type](rewardOut, register).get, Height)
    val heightIncreased = GT(Height, ExtractRegisterAs[SLong.type](Self, register).get)
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)
    val outputsNum = EQ(SizeOf(Outputs), 2)
    val correctMinerProposition = EQ(ExtractScriptBytes(minerOut),
      Append(ConcreteCollection(OpCodes.ProveDlogCode, SGroupElement.typeCode), MinerPubkey))

    val prop = AND(
      heightIncreased,
      correctMinerProposition,
      BinOr(AND(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins)
    )

    val env = Map("fixedRatePeriod" -> s.fixedRatePeriod,
      "epochLength" -> s.epochLength,
      "fixedRate" -> s.fixedRate,
      "oneEpochReduction" -> s.oneEpochReduction)

    val prop1 = compile(env,
      """{
        |    val epoch = 1 + ((HEIGHT - fixedRatePeriod) / epochLength)
        |    val out = OUTPUTS(0)
        |    val minerOut = OUTPUTS(1)
        |    val coinsToIssue = if(HEIGHT < fixedRatePeriod) fixedRate else fixedRate - (oneEpochReduction * epoch)
        |    val correctCoinsConsumed = coinsToIssue == (SELF.value - out.value)
        |    val sameScriptRule = SELF.propositionBytes == out.propositionBytes
        |    val heightIncreased = HEIGHT > SELF.R4[Long].get
        |    val heightCorrect = out.R4[Long].get == HEIGHT
        |    val lastCoins = SELF.value <= oneEpochReduction
        |    val outputsNum = OUTPUTS.size == 2
        |    val correctMinerProposition = minerOut.propositionBytes == Array[Byte](-51.toByte, 7.toByte) ++ MinerPubkey
        |    allOf(Array(heightIncreased, correctMinerProposition, allOf(Array(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect)) || lastCoins))
        |}""".stripMargin).asBoolValue

    prop1 shouldEqual prop

    val minerImage = prover.dlogSecrets.head.publicImage
    val minerPubkey = minerImage.pkBytes
    val minerProp = minerImage

    val initialBoxCandidate: ErgoBox = ErgoBox(coinsTotal, prop, Seq(), Map(register -> LongConstant(-1)))
    val initBlock = BlockchainSimulationSpecification.Block(
      IndexedSeq(
        ErgoLikeTransaction(
          IndexedSeq(),
          IndexedSeq(initialBoxCandidate)
        )
      ),
      minerPubkey
    )
    val genesisState = ValidationState.initialState(initBlock)
    val fromState = genesisState.boxesReader.byId(genesisState.boxesReader.allIds.head).get
    val initialBox = ErgoBox(initialBoxCandidate.value, initialBoxCandidate.proposition,
      initialBoxCandidate.additionalTokens, initialBoxCandidate.additionalRegisters, initBlock.txs.head.id, 0)
    initialBox shouldBe fromState

    def genCoinbaseLikeTransaction(state: ValidationState,
                                   emissionBox: ErgoBox,
                                   height: Long): ErgoLikeTransaction = {
      assert(state.state.currentHeight == height - 1)
      val ut = if (emissionBox.value > s.oneEpochReduction) {
        val minerBox = new ErgoBoxCandidate(emissionAtHeight(height), minerProp, Seq(), Map())
        val newEmissionBox: ErgoBoxCandidate =
          new ErgoBoxCandidate(emissionBox.value - minerBox.value, prop, Seq(), Map(register -> LongConstant(height)))

        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(newEmissionBox, minerBox)
        )
      } else {
        val minerBox1 = new ErgoBoxCandidate(emissionBox.value - 1, minerProp, Seq(), Map(register -> LongConstant(height)))
        val minerBox2 = new ErgoBoxCandidate(1, minerProp, Seq(), Map(register -> LongConstant(height)))
        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(minerBox1, minerBox2)
        )
      }

      val context = ErgoLikeContext(height,
        state.state.lastBlockUtxoRoot,
        minerPubkey,
        IndexedSeq(emissionBox),
        ut,
        emissionBox,
        ContextExtension.empty)
      val proverResult = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, context, ut.messageToSign).get
      ut.toSigned(IndexedSeq(proverResult))
    }

    val st = System.currentTimeMillis()

    def chainGen(state: ValidationState,
                 emissionBox: ErgoBox,
                 height: Int,
                 hLimit: Int): Unit = if (height < hLimit) {
      if (height % 1000 == 0) {
        println(s"block $height in ${System.currentTimeMillis() - st} ms, ${emissionBox.value} coins remain")
      }
      val tx = genCoinbaseLikeTransaction(state, emissionBox, height)
      val block = Block(IndexedSeq(tx), minerPubkey)
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
