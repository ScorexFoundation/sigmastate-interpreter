package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.R3
import org.ergoplatform._
import scorex.utils.ScryptoLogging
import sigmastate.Values.LongConstant
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.BlockchainSimulationSpecification.{Block, ValidationState}
import sigmastate.utxo._
import sigmastate.{SLong, _}

/**
  * Coin emission specification.
  * Instead of having implicit emission via coinbase transaction, we implement 1 output in a state with script,
  * that controls emission rules
  */
class CoinEmissionSpecification extends SigmaTestingCommons with ScryptoLogging {

  // Some constants
  private val coinsInOneErgo: Long = 100000000
  private val blocksPerHour: Int = 30
  private val blocksPerYear: Int = 365 * 24 * blocksPerHour
  private val blocksTotal: Int = blocksPerYear * 8
  private val rewardReductionPeriod: Long = 90 * 24 * blocksPerHour
  private val fixedRatePeriod: Long = 2 * blocksPerYear - rewardReductionPeriod
  private val fixedRate: Long = 2250 * coinsInOneErgo / blocksPerHour
  private val decreasingEpochs: Long = (blocksTotal - fixedRatePeriod) / rewardReductionPeriod


  def emissionAtHeight(h: Long): Long = {
    if (h <= fixedRatePeriod) {
      fixedRate
    } else if (h > blocksTotal) {
      0
    } else {
      fixedRate - fixedRate * ((h - fixedRatePeriod) / rewardReductionPeriod) / decreasingEpochs
    }
  }.ensuring(_ >= 0, s"Negative at $h")

  property("emission specification") {
    val register = R3
    val prover = new ErgoLikeProvingInterpreter()

    val red = Modulo(Multiply(fixedRate, Modulo(Minus(Height, fixedRatePeriod), rewardReductionPeriod)), decreasingEpochs)
    val coinsToIssue = If(LE(Height, fixedRatePeriod), fixedRate, Minus(fixedRate, red))
    val out = ByIndex(Outputs, 0)
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(out))
    val heightCorrect = EQ(ExtractRegisterAs[SLong.type](out, register), Height)
    val heightIncreased = GT(ExtractRegisterAs[SLong.type](out, register), ExtractRegisterAs[SLong.type](Self, register))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(out)))

    val prop = OR(AND(sameScriptRule, correctCoinsConsumed, heightIncreased, heightCorrect), EQ(Height, blocksTotal))
    val minerProp = prover.dlogSecrets.head.publicImage

    val initialBoxCandidate: ErgoBox = ErgoBox(9773992500000000L, prop, Map(register -> LongConstant(-1)))
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
      initialBoxCandidate.additionalRegisters, initBlock.txs.head.id, 0)
    initialBox shouldBe fromState

    def genCoinbaseLikeTransaction(state: ValidationState,
                                   emissionBox: ErgoBox,
                                   height: Long): ErgoLikeTransaction = {
      assert(state.state.currentHeight == height - 1)
      val ut = if (height != blocksTotal) {
        val minerBox = new ErgoBoxCandidate(emissionAtHeight(height), minerProp, Map())
        val newEmissionBox: ErgoBoxCandidate =
          new ErgoBoxCandidate(emissionBox.value - minerBox.value, prop, Map(register -> LongConstant(height)))

        UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(emissionBox.id)),
          IndexedSeq(newEmissionBox, minerBox)
        )
      } else {
        val minerBox = new ErgoBoxCandidate(emissionBox.value, minerProp, Map(register -> LongConstant(height)))
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
        ContextExtension.empty)
      val proverResult = prover.prove(prop, context, ut.messageToSign).get
      ut.toSigned(IndexedSeq(proverResult))
    }

    val st = System.currentTimeMillis()

    def chainGen(state: ValidationState,
                 emissionBox: ErgoBox,
                 height: Int,
                 hLimit: Int): Unit = if (height < hLimit) {
      if (height % 10000 == 0) log.debug(s"block $height from $blocksTotal." +
        s" ${height.toDouble / blocksTotal}% in ${System.currentTimeMillis() - st} ms")
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

//    chainGen(genesisState, initialBox, 2, 100000000)
    chainGen(genesisState, initialBox, 2, 100)
  }
}