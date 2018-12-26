package org.ergoplatform

import sigmastate.Values.ErgoTree.ConstantSegregationHeader
import sigmastate.interpreter.CryptoConstants
import sigmastate.utxo.{ExtractCreationInfo, SelectField}
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{LongConstant, Value, ByteArrayConstant, IntConstant, ErgoTree, ConcreteCollection}
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.lang.Terms.ValueOps

object ErgoScriptPredef {

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


}
