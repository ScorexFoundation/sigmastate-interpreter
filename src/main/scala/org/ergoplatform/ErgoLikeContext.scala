package org.ergoplatform

import org.ergoplatform.ErgoLikeContext.Height
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.CostTable.Cost

import scala.util.Try

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

// todo: write description
class ErgoLikeContext(val currentHeight: Height,
                      val lastBlockUtxoRoot: AvlTreeData,
                      val boxesToSpend: IndexedSeq[ErgoBox],
                      val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                      val self: ErgoBox,
                      val extension: ContextExtension = ContextExtension(Map())
                 ) extends Context[ErgoLikeContext] {
  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, spendingTransaction, self, newExtension)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, newSpendingTransaction, self, extension)
}

object ErgoLikeContext {
  type Height = Long

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox,
            extension: ContextExtension = ContextExtension(Map())) =
    new ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, spendingTransaction, self, extension)


  def dummy(selfDesc: ErgoBox) = ErgoLikeContext(currentHeight = 0,
    lastBlockUtxoRoot = AvlTreeData.dummy, boxesToSpend = IndexedSeq(),
    spendingTransaction = null, self = selfDesc)

  def fromTransaction(tx: ErgoLikeTransaction,
                      blockchainState: BlockchainState,
                      boxesReader: ErgoBoxReader,
                      inputIndex: Int): Try[ErgoLikeContext] = Try {

    val boxes = tx.inputs.map(_.boxId).map(id => boxesReader.byId(id).get)

    val proverExtension = tx.inputs(inputIndex).spendingProof.extension

    ErgoLikeContext(blockchainState.currentHeight,
      blockchainState.lastBlockUtxoRoot,
      boxes,
      tx,
      boxes(inputIndex),
      proverExtension)
  }
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueLong {
  override val opCode: OpCode = OpCodes.HeightCode

  override def cost[C <: Context[C]](context: C): Long = 2 * Cost.IntConstantDeclaration

  def opType = SFunc(SContext, SInt)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.InputsCode

  override def cost[C <: Context[C]](context: C) =
    context.asInstanceOf[ErgoLikeContext].boxesToSpend.map(_.cost).sum + Cost.ConcreteCollectionDeclaration

  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.OutputsCode

  override def cost[C <: Context[C]](context: C) =
    context.asInstanceOf[ErgoLikeContext].spendingTransaction.outputs.map(_.cost).sum + Cost.ConcreteCollectionDeclaration

  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree {
  override val opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode

  override def cost[C <: Context[C]](context: C) = Cost.AvlTreeConstantDeclaration + 1
  def opType = SFunc(SContext, tpe)
}


/** When interpreted evaluates to a BoxConstant built from Context.self */
case object Self extends NotReadyValueBox {
  override val opCode: OpCode = OpCodes.SelfCode

  override def cost[C <: Context[C]](context: C) = context.asInstanceOf[ErgoLikeContext].self.cost

  def opType = SFunc(SContext, SBox)
}
