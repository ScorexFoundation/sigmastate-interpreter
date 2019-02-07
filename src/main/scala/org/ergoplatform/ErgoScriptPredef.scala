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
    * @param delta           - number of blocks for which miner should hold this box before spending it
    * @param minerPkBytesVal - byte array val for pk to substitute in the reward script
    */
  def expectedMinerOutScriptBytesVal(delta: Int, minerPkBytesVal: Value[SByteArray]): Value[SByteArray] = {
    val genericPk = ProveDlog(CryptoConstants.dlogGroup.generator)
    val genericMinerProp = rewardOutputScript(delta, genericPk)
    val genericMinerPropBytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(genericMinerProp)
    val expectedGenericMinerProp = rewardOutputScript(delta, genericPk)

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
    * Proposition that allows to send coins to a box which is protected by the following proposition:
    * prove dlog of miner's public key and height is at least `delta` blocks bigger then the current one.
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
    * A contract that only allows to collect emission reward by a box with miner proposition.
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
    * The script allows to spend a box, if:
    * - first transaction output contains at least EmissionRules.remainingFoundationAtHeight coins in it
    * - first transaction output is protected by this script
    * - conditions from the first non-mandatory register (R4) are satisfied
    *
    * Thus, this script always controls the level of emission and does not allow to take
    * more coinы than prescribed by emission rules. In addition, it is protected by
    * custom proposition in R4 which is assumed to be a simple 2-of-3 multisignature with
    * public keys of foundation members in the beginning. When foundation members spend
    * this box, they are free to put any new proposition to the R4 register, thus they
    * may add or remove members, or change it to something more complicated like
    * `tokenThresholdScript`.
    */
  def foundationScript(s: MonetarySettings): Value[SBoolean.type] = {
    // new output of the foundation
    val newFoundationBox = ByIndex(Outputs, IntConstant(0))
    // calculate number of coins, that are not issued yet and should be kept in `newFoundationBox`
    // the same as Emission.remainingFoundationRewardAtHeight rewritten in Ergo script
    val remainingAmount = {
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
    // check, that `newFoundationBox` contains at least `remainingAmount`
    val amountCorrect = GE(ExtractAmount(newFoundationBox), remainingAmount)
    // check, that `newFoundationBox` have the same protecting script
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(newFoundationBox))
    // check, that additional rules defined by foundation members are satisfied
    val customProposition = DeserializeRegister(ErgoBox.R4, SBoolean)
    // combine 3 conditions above with AND conjunction
    AND(amountCorrect, sameScriptRule, customProposition)
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
