package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values.{LongConstant, FuncValue, Value, ByteArrayConstant, IntConstant, ErgoTree, ValUse, ConcreteCollection}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.{TransformingSigmaBuilder, SigmaCompiler}
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.{ExtractRegisterAs, _}
import sigmastate.{SLong, _}

object ErgoScriptPredef {
  import sigmastate.interpreter.Interpreter._
  val compiler = new SigmaCompiler(TransformingSigmaBuilder)

  def compileWithCosting(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
    val interProp = compiler.typecheck(env, code)
    val IR.Pair(calcF, _) = IR.doCosting(env, interProp)
    IR.buildTree(calcF)
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

  def rewardOutputScriptForCurrentMiner(delta: Int): Value[SByteArray] = {
    val expectedBytes = rewardOutputScript(delta).bytes
    val currentMinerScript = SubstConstants(
      ByteArrayConstant(expectedBytes),
      ConcreteCollection(IntConstant(0)),
      ConcreteCollection(MinerPubkey))
    currentMinerScript
  }

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

  /**
    * Proposition of the box, that may be taken by such transaction,
    * which inputs contains at least `tokenAmount` of token with id `tokenId`.
    */
  def tokenThreshold(tokenId: Array[Byte], tokenAmount: Long): Value[SBoolean.type] = {
    val mapper: Value[SFunc] = FuncValue(Vector((1, SBox)), getTokenAmount(ValUse(1, SBox), tokenId))
    GE(Fold.sum[SLong.type](MapCollection(Inputs, mapper)), tokenAmount)
  }

  /**
    * Return amount of token with id `tokenId` in the box
    */
  def getTokenAmount(box: Value[SBox.type], tokenId: Array[Byte]): Fold[SLong.type, SLong.type] = {
    val tokens = getTokens(box)
    val tokenPair = ValUse(3, STuple(SByteArray, SLong))
    val ourTokenAmount: Value[SLong.type] = If(
      EQ(SelectField(tokenPair, 1), tokenId),
      SelectField(tokenPair, 2).asLongValue,
      LongConstant(0)
    )
    val mapper: Value[SFunc] = FuncValue(3, STuple(SByteArray, SLong), ourTokenAmount)
    Fold.sum[SLong.type](MapCollection(tokens, mapper))
  }

  /**
    * Return collection with all tokens of the box
    */
  def getTokens(box: Value[SBox.type]): Value[SCollection[STuple]] = {
    ExtractRegisterAs(box, ErgoBox.TokensRegId)(ErgoBox.STokensRegType).get
  }

}
