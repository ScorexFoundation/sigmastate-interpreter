package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.R4
import org.ergoplatform._
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Blake2b256
import scorex.utils.ScryptoLogging
import sigmastate.Values.IntConstant
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.BlockchainSimulationSpecification.{Block, ValidationState}
import sigmastate.utxo._
import sigmastate.{SInt, _}

class CoinEmissionSpecification extends SigmaTestingCommons with ScryptoLogging {

  // Some constants
  private val coinsInOneErgo: Long = 100000000
  private val blocksPerHour: Int = 30
  private val blocksPerYear: Int = 365 * 24 * blocksPerHour
  private val blocksTotal: Int = blocksPerYear * 8
  private val rewardReductionPeriod: Int = 90 * 24 * blocksPerHour
  private val fixedRatePeriod = 2 * blocksPerYear - rewardReductionPeriod
  private val fixedRate = 2250 * coinsInOneErgo / blocksPerHour
  private val decreasingEpochs = (blocksTotal - fixedRatePeriod) / rewardReductionPeriod


  def emissionAtHeight(h: Long): Long = {
//    if (h <= fixedRatePeriod) {
//      fixedRate
//    } else if (h > blocksTotal) {
//      0
//    } else {
//      val epoch: Int = ((h - fixedRatePeriod) / rewardReductionPeriod).toInt
//      fixedRate - fixedRate * epoch / decreasingEpochs
//    }
    1
  }.ensuring(_ >= 0, s"Negative at $h")


  /**
    * Coin emission specification.
    * Instead of having implicit emission via coinbase transaction, it is possible to have 1 output in a state,
    * that controls emission rules
    */
  property("emission specification") {
    val prover = new ErgoLikeProvingInterpreter()

    val coinsToIssue = IntConstant(1)
    val out = ByIndex(Outputs, 0)
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(out))
    val heightCorrect = EQ(ExtractRegisterAs[SInt.type](out, R4), Height)
    val heightIncreased = GT(ExtractRegisterAs[SInt.type](out, R4), ExtractRegisterAs[SInt.type](Self, R4))
    val correctCoinsConsumed = EQ(ExtractAmount(out), Minus(ExtractAmount(Self), coinsToIssue))

    val prop = AND(sameScriptRule, correctCoinsConsumed, heightIncreased, heightCorrect)
    val minerProp = prover.dlogSecrets.head.publicImage

    val initialBoxCandidate:ErgoBox = ErgoBox(9773992500000000L, prop, Map(R4 -> IntConstant(-1)))
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
                                   height: Int): ErgoLikeTransaction = {
      val minerBox = new ErgoBoxCandidate(emissionAtHeight(height), minerProp, Map())
      val newEmissionBox = new ErgoBoxCandidate(emissionBox.value - minerBox.value, prop, Map(R4 -> IntConstant(height)))

      val ut = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(emissionBox.id)),
        IndexedSeq(newEmissionBox, minerBox)
      )
      val context = ErgoLikeContext(height,
        state.state.lastBlockUtxoRoot,
        IndexedSeq(emissionBox),
        ut,
        emissionBox,
        ContextExtension.empty)
      val proverResult = prover.prove(prop, context, ut.messageToSign).get
      ut.toSigned(IndexedSeq(proverResult))
    }


    def chainGen(state: ValidationState,
                 emissionBox: ErgoBox,
                 height: Int,
                 hLimit: Int): Unit = if (height < hLimit) {
      val tx = genCoinbaseLikeTransaction(state, emissionBox, height)
      val block = Block(IndexedSeq(tx))
      val newState = state.applyBlock(block).get
      val newEmissionBox = newState.boxesReader.byId(tx.outputs.head.id).get
      chainGen(newState, newEmissionBox, height + 1, hLimit)
    }

    chainGen(genesisState, initialBox, 2, 100)
  }
}