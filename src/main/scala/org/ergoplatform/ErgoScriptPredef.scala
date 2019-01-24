package org.ergoplatform

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.settings.MonetarySettings
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{IntArrayConstant, IntConstant, LongConstant, SigmaPropValue, Value}
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.IRContext
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.utxo._

object ErgoScriptPredef {

  import sigmastate.interpreter.Interpreter._

  def compileWithCosting(env: ScriptEnv, code: String, networkPrefix: NetworkPrefix)(implicit IR: IRContext): Value[SType] = {
    val compiler = new SigmaCompiler(networkPrefix, TransformingSigmaBuilder)
    val interProp = compiler.typecheck(env, code)
    val IR.Pair(calcF, _) = IR.doCosting(env, interProp)
    IR.buildTree(calcF)
  }

  /**
    * Byte array value of the serialized reward output script proposition with pk being substituted
    * with given pk
    *
    * @param delta           - number of blocks miner should hold this box before spending it
    * @param minerPkBytesVal - byte array val for pk to substitute in the reward script
    */
  def expectedMinerOutScriptBytesVal(delta: Int, minerPkBytesVal: Value[SByteArray]): Value[SByteArray] = {
    val genericPk = ProveDlog(CryptoConstants.dlogGroup.generator)
    val genericMinerProp = rewardOutputScript(delta, genericPk)
    val genericMinerPropBytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(genericMinerProp)
    val expectedGenericMinerProp = AND(
      GE(Height, Plus(boxCreationHeight(Self), IntConstant(delta))),
      genericPk
    )
    assert(genericMinerProp == expectedGenericMinerProp, s"reward output script changed, check and update constant position for substitution below")
    // first segregated constant is delta, so key is second constant
    val positions = IntArrayConstant(Array[Int](1))
    val minerPubkeySigmaProp = ProveDlog(DecodePoint(minerPkBytesVal))
    val newVals = Values.ConcreteCollection(Vector[SigmaPropValue](minerPubkeySigmaProp), SSigmaProp)
    SubstConstants(genericMinerPropBytes, positions, newVals)
  }

  /**
    * Required script of the box, that collects mining rewards
    */
  def rewardOutputScript(delta: Int, minerPk: ProveDlog): Value[SBoolean.type] = {
    AND(
      GE(Height, Plus(boxCreationHeight(Self), IntConstant(delta))),
      minerPk
    )
  }

  /**
    * Proposition, that allows to send coins to a box, that is protected by the following proposition:
    * prove dlog of miners public key and height is at least `delta` blocks bigger then the current one
    */
  def feeProposition(delta: Int = 720): Value[SBoolean.type] = {
    val out = ByIndex(Outputs, IntConstant(0))
    AND(
      EQ(Height, boxCreationHeight(out)),
      EQ(ExtractScriptBytes(out), expectedMinerOutScriptBytesVal(delta, MinerPubkey)),
      EQ(SizeOf(Outputs), 1)
    )
  }

  /**
    * Proposition box, that only allows to collect a part of all coins
    * to a box with miner proposition.
    */
  def emissionBoxProp(s: MonetarySettings): Value[SBoolean.type] = {
    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val minersReward = s.fixedRate - s.foundersInitialReward
    val minersFixedRatePeriod = s.fixedRatePeriod + 2 * s.epochLength
    val epoch = Plus(IntConstant(1), Divide(Minus(Height, IntConstant(s.fixedRatePeriod)), IntConstant(s.epochLength)))
    val coinsToIssue = If(LT(Height, IntConstant(minersFixedRatePeriod)),
      minersReward,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch.upcastTo(SLong)))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(boxCreationHeight(rewardOut), Height)
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)
    val outputsNum = EQ(SizeOf(Outputs), 2)

    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(s.minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )
    AND(
      heightIncreased,
      correctMinerOutput,
      OR(AND(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins)
    )
  }

  /**
    * Script for Ergo foundation box.
    *
    * The script allows to collect coins, if:
    * - First transaction output contains at least EmissionRules.remainingFoundationAtHeight coins in it
    * and is protected by the same script AND
    * - satisfies conditions from the first non-mandatory register
    */
  def foundationScript(s: MonetarySettings): Value[SBoolean.type] = {
    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val remainingAmount = {
      // Emission.remainingFoundationRewardAtHeight in Ergo script
      val full15reward = (s.foundersInitialReward - 2 * s.oneEpochReduction) * s.epochLength
      val full45reward = (s.foundersInitialReward - s.oneEpochReduction) * s.epochLength
      val fixedRatePeriodMinus1: Int = s.fixedRatePeriod - 1

      If(LT(Height, IntConstant(s.fixedRatePeriod)),
        Plus(
          LongConstant(full15reward + full45reward),
          Multiply(s.foundersInitialReward, Upcast(Minus(fixedRatePeriodMinus1, Height), SLong))
        ),
        If(LT(Height, IntConstant(s.fixedRatePeriod + s.epochLength)),
          Plus(
            full15reward,
            Multiply(
              s.foundersInitialReward - s.oneEpochReduction,
              Upcast(Minus(fixedRatePeriodMinus1 + s.epochLength, Height), SLong)
            )
          ),
          If(LT(Height, IntConstant(s.fixedRatePeriod + 2 * s.epochLength)),
            Multiply(
              s.foundersInitialReward - 2 * s.oneEpochReduction,
              Upcast(Minus(fixedRatePeriodMinus1 + 2 * s.epochLength, Height), SLong)
            ),
            LongConstant(0)
          )
        )
      )
    }
    val amountCorrect = GE(ExtractAmount(rewardOut), remainingAmount)
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    AND(amountCorrect, sameScriptRule, DeserializeRegister(ErgoBox.R4, SBoolean))
  }

  /**
    * Creation height of a box
    */
  def boxCreationHeight(box: Value[SBox.type]): Value[SInt.type] =
    SelectField(ExtractCreationInfo(box), 1).asIntValue

  /**
    * Proposition of the box that may be spent by a transaction
    * which inputs contains at least `thresholdAmount` of token with id `tokenId`.
    * The logic of this script is following
    * (v1) INPUTS.flatMap(box => box.tokens.filter(t => t._1 == tokenId).map(t => t._2)).sum >= thresholdAmount
    * (v2) INPUTS.flatMap(box => box.tokens).filter(t => t._1 == tokenId).sum >= thresholdAmount
    * (v3) INPUTS.map(box => box.tokens.find(t => t._1 == tokenId).map(t => t._2).getOrElse(0)).sum >= thresholdAmount
    */
  def tokenThresholdScript(tokenId: Array[Byte], thresholdAmount: Long, networkPrefix: NetworkPrefix)(implicit IR: IRContext): Value[SBoolean.type] = {
    val env = emptyEnv + ("tokenId" -> tokenId, "thresholdAmount" -> thresholdAmount)
    val res = compileWithCosting(env,
      """{
        |  val sumValues = { (xs: Coll[Long]) => xs.fold(0L, { (acc: Long, amt: Long) => acc + amt }) }
        |
        |  val tokenAmounts = INPUTS.map({ (box: Box) =>
        |    val tokens = box.R2[Coll[(Coll[Byte], Long)]].get
        |    sumValues(tokens.map { (tokenPair: (Coll[Byte], Long)) =>
        |      val ourTokenAmount = if (tokenPair._1 == tokenId) tokenPair._2 else 0L
        |      ourTokenAmount
        |    })
        |  })
        |  val total = sumValues(tokenAmounts)
        |  total >= thresholdAmount
        |}
      """.stripMargin, networkPrefix)
    res.asBoolValue
  }

}
