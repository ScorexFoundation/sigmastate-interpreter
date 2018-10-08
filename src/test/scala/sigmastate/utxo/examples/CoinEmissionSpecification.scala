package sigmastate.utxo.examples

import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform.ErgoLikeContext.Metadata._
import org.ergoplatform.{ErgoLikeContext, Height, _}
import scorex.util.ScorexLogging
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.Terms._
import sigmastate.utxo.BlockchainSimulationSpecification.{Block, ValidationState}
import sigmastate.utxo._
import sigmastate.{SLong, _}

/**
  * Coin emission specification.
  * Instead of having implicit emission via coinbase transaction, we implement 1 output in a state with script,
  * that controls emission rules
  */
class CoinEmissionSpecification extends SigmaTestingCommons with ScorexLogging {

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
    val prover = new ErgoLikeProvingInterpreter()

    val out = ByIndex(Outputs, IntConstant(0))
    val epoch = Plus(LongConstant(1), Divide(Minus(Height, LongConstant(s.fixedRatePeriod)), LongConstant(s.epochLength)))
    val coinsToIssue = If(LT(Height, LongConstant(s.fixedRatePeriod)),
      s.fixedRate,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(out))
    val heightCorrect = EQ(ExtractRegisterAs[SLong.type](out, register).get, Height)
    val heightIncreased = GT(Height, ExtractRegisterAs[SLong.type](Self, register).get)
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(out)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)

    val prop = OR(AND(correctCoinsConsumed, heightCorrect, heightIncreased, sameScriptRule), AND(heightIncreased, lastCoins))

    val env = Map("fixedRatePeriod" -> s.fixedRatePeriod,
      "epochLength" -> s.epochLength,
      "fixedRate" -> s.fixedRate,
      "oneEpochReduction" -> s.oneEpochReduction)
    val prop1 = compile(env,
      """{
        |    val epoch = 1 + ((HEIGHT - fixedRatePeriod) / epochLength)
        |    val out = OUTPUTS(0)
        |    val coinsToIssue = if(HEIGHT < fixedRatePeriod) fixedRate else fixedRate - (oneEpochReduction * epoch)
        |    val correctCoinsConsumed = coinsToIssue == (SELF.value - out.value)
        |    val sameScriptRule = SELF.propositionBytes == out.propositionBytes
        |    val heightIncreased = HEIGHT > SELF.R4[Long].get
        |    val heightCorrect = out.R4[Long].get == HEIGHT
        |    val lastCoins = SELF.value <= oneEpochReduction
        |    allOf(Array(correctCoinsConsumed, heightCorrect, heightIncreased, sameScriptRule)) || (heightIncreased && lastCoins)
        |}""".stripMargin).asBoolValue

    prop1 shouldEqual prop

    val minerProp = prover.dlogSecrets.head.publicImage

    val initialBoxCandidate: ErgoBox = ErgoBox(coinsTotal, prop, Seq(), Map(register -> LongConstant(-1)))
    val initBlock = BlockchainSimulationSpecification.Block {
      IndexedSeq(
        ErgoLikeTransaction(
          IndexedSeq(),
          IndexedSeq(initialBoxCandidate)
        )
      )
    }
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
        val minerBox = new ErgoBoxCandidate(emissionBox.value, minerProp, Seq(), Map(register -> LongConstant(height)))
        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(minerBox)
        )
      }

      val context = ErgoLikeContext(height,
        state.state.lastBlockUtxoRoot,
        IndexedSeq(emissionBox),
        ut,
        emissionBox,
        metadata = Metadata(MainnetNetworkPrefix),
        ContextExtension.empty)
      val proverResult = prover.prove(prop, context, ut.messageToSign).get
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
      val block = Block(IndexedSeq(tx))
      val newState = state.applyBlock(block).get
      if (tx.outputs.length == 2) {
        val newEmissionBox = newState.boxesReader.byId(tx.outputs.head.id).get
        chainGen(newState, newEmissionBox, height + 1, hLimit)
      } else {
        log.debug(s"Emission box is consumed at height $height")
      }
    }

    chainGen(genesisState, initialBox, 0, 100000000)
  }
}
