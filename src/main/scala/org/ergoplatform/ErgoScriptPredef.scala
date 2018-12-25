package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ByteArrayConstant, ConcreteCollection, ErgoTree, FuncValue, IntConstant, LongConstant, SValue, ValUse, Value}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.{ExtractRegisterAs, _}
import sigmastate.{SLong, _}

object ErgoScriptPredef {

  /**
    * Required script of the box, that collects mining rewards
    */
  def rewardOutputScript(delta: Int): ErgoTree = {
    import ErgoTree._
    val createdAtHeight = SelectField(ExtractCreationInfo(Self), 1).asLongValue
    val root = AND(
      GE(Height, Plus(createdAtHeight, LongConstant(delta))),
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
  def getTokenAmount(box: Value[SBox.type], tokenId: Array[Byte]) = {
    val tokens = getTokens(box)
    val tokenPair = ValUse(3, STuple(SByteArray, SLong))
    val ourTokenAmount: SValue = If(EQ(SelectField(tokenPair, 1), tokenId), SelectField(tokenPair, 2).asLongValue, 0)
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
