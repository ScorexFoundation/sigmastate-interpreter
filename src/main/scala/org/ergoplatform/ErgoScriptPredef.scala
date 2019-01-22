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

  /**
    * Proposition of the box, that may be taken by a transaction,
    * which inputs contains at least `thresholdAmount` of token with id `tokenId`.
    * The logic of this script is the following
    * (v1) INPUTS.flatMap(box => box.tokens.filter(t => t._1 == tokenId).map(t => t._2)).sum >= thresholdAmount
    * (v2) INPUTS.flatMap(box => box.tokens).filter(t => t._1 == tokenId).sum >= thresholdAmount
    * (v3) INPUTS.map(box => box.tokens.find(t => t._1 == tokenId).map(t => t._2).getOrElse(0)).sum >= thresholdAmount
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
