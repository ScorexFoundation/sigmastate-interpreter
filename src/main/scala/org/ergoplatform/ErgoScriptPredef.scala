package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ByteArrayConstant, ConcreteCollection, ErgoTree, IntArrayConstant, IntConstant, LongConstant, SigmaPropValue, Value}
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
  val compiler = new SigmaCompiler(TransformingSigmaBuilder)

  def compileWithCosting(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
    val interProp = compiler.typecheck(env, code)
    val IR.Pair(calcF, _) = IR.doCosting(env, interProp)
    IR.buildTree(calcF)
  }

  /**
    * Byte array value of the serialized reward output script proposition with pk being substituted
    * with given pk
    *
    * @param delta - number of blocks miner should hold this box before spending it
    * @param minerPkBytesVal - byte array val for pk to substitute in the reward script
    */
  def expectedMinerOutScriptBytesVal(delta: Int, minerPkBytesVal: Value[SByteArray]): Value[SByteArray] = {
    val genericPk = ProveDlog(CryptoConstants.dlogGroup.generator)
    val genericMinerProp = rewardOutputScriptWithPk(delta, genericPk)
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
  def rewardOutputScript(delta: Int): ErgoTree = {
    import ErgoTree._
    val createdAtHeight = SelectField(ExtractCreationInfo(Self), 1).asLongValue
    val root = AND(
      GE(Height, Plus(createdAtHeight, IntConstant(delta))),
      ProveDlog(DecodePoint(Values.ConstantPlaceholder(0, SByteArray)))
    )
    ErgoTree(ConstantSegregationHeader, Vector(ByteArrayConstant(Array.emptyByteArray)), root)
  }

  /**
    * TODO leave one of rewardOutputScript and rewardOutputScriptWithPk
    *
    * Required script of the box, that collects mining rewards
    */
  def rewardOutputScriptWithPk(delta: Int, minerPk: ProveDlog): Value[SBoolean.type] = {
    AND(
      GE(Height, Plus(SelectField(ExtractCreationInfo(Self), 1).asLongValue, LongConstant(delta))),
      minerPk
    )
  }

  def rewardOutputScriptForCurrentMiner(delta: Int): Value[SByteArray] = {
    val expectedBytes = rewardOutputScript(delta).bytes
    val currentMinerScript = SubstConstants(
      ByteArrayConstant(expectedBytes),
      ConcreteCollection(IntConstant(0)),
      ConcreteCollection(MinerPubkey))
    currentMinerScript
  }

  /**
    * Proposition, that allows to send coins to a box, that is protected by the following proposition:
    * prove dlog of miners public key and height is at least `delta` blocks bigger then the current one
    */
  def feeProposition(delta: Int = 720): Value[SBoolean.type] = {
    val out = ByIndex(Outputs, IntConstant(0))
    AND(
      EQ(Height, SelectField(ExtractCreationInfo(out), 1).asLongValue),
      EQ(ExtractScriptBytes(out), expectedMinerOutScriptBytesVal(delta, MinerPubkey)),
      EQ(SizeOf(Outputs), 1)
    )
  }

  /**
    * Proposition box, that only allows to collect a part of all coins
    * to a box with miner proposition.
    */
  def emissionBoxProp(fixedRatePeriod: Long,
                      epochLength: Int,
                      fixedRate: Long,
                      oneEpochReduction: Long,
                      minerRewardDelay: Int): Value[SBoolean.type] = {
    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val epoch = Plus(IntConstant(1), Divide(Minus(Height, IntConstant(fixedRatePeriod.toInt)), IntConstant(epochLength)))
    val coinsToIssue = If(LT(Height, IntConstant(fixedRatePeriod.toInt)),
      fixedRate,
      Minus(fixedRate, Multiply(oneEpochReduction, epoch.upcastTo(SLong)))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(boxCreationHeight(rewardOut), Height)
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), oneEpochReduction)
    val outputsNum = EQ(SizeOf(Outputs), 2)

    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )
    AND(
      heightIncreased,
      correctMinerOutput,
      OR(AND(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins)
    )
  }

  /**
    * Creation height of a box
    */
  def boxCreationHeight(box: Value[SBox.type]): Value[SInt.type] = SelectField(ExtractCreationInfo(box), 1).asIntValue

  /**
    * Proposition of the box, that may be taken by a transaction,
    * which inputs contains at least `thresholdAmount` of token with id `tokenId`.
    */
  def tokenThresholdScript(tokenId: Array[Byte], thresholdAmount: Long)(implicit IR: IRContext): Value[SBoolean.type] = {
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
      """.stripMargin )
    res.asBoolValue
  }

}
