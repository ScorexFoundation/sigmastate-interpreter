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

  // 1 weeks of fixed rate
  private val fixedRatePeriod =  blocksPerHour * 24 * 7
  // 15 coins per block
  private val fixedRate = 15 * coinsInOneErgo
  // 1 days epoch
  private val epochLength: Int = 24 * blocksPerHour
  // 3 coins reduction every epoch
  private val oneEpochReduction = 3 * coinsInOneErgo

  val (coinsTotal, blocksTotal) = {
    def loop(height: Int, acc: Long): (Long, Int) = {
      val currentRate = emissionAtHeight(height)
      if(currentRate > 0) {
        loop(height + 1, acc + currentRate)
      } else {
        (acc, height - 1)
      }
    }
    loop(0, 0)
  }


  def emissionAtHeight(h: Long): Long = {
    if (h < fixedRatePeriod) {
      fixedRate
    } else {
      val epoch = 1 + (h - fixedRatePeriod) / epochLength
      Math.max(fixedRate - oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  property("emission specification") {
    val register = R3
    val prover = new ErgoLikeProvingInterpreter()

    val epoch = Plus(1, Divide(Minus(Height, LongConstant(fixedRatePeriod)), epochLength))
    val coinsToIssue = If(LT(Height, LongConstant(fixedRatePeriod)),
      fixedRate,
      Minus(fixedRate, Multiply(oneEpochReduction, epoch))
    )

    val out = ByIndex(Outputs, 0)
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(out))
    val heightCorrect = EQ(ExtractRegisterAs[SLong.type](out, register), Height)
    val heightIncreased = GT(Height, ExtractRegisterAs[SLong.type](Self, register))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(out)))
    val lastCoins = LE(ExtractAmount(Self), oneEpochReduction)

    val prop = AND(heightIncreased, OR(AND(sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins))
    val minerProp = prover.dlogSecrets.head.publicImage

    val initialBoxCandidate: ErgoBox = ErgoBox(coinsTotal, prop, Map(register -> LongConstant(-1)))
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
      val ut = if (emissionBox.value > oneEpochReduction) {
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
      if(height % 1000 == 0) {
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