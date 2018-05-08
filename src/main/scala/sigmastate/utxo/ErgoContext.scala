package sigmastate.utxo

import sigmastate._
import Values._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.ErgoContext.Height

import scala.util.Try

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

// todo: write description
case class ErgoContext(currentHeight: Height,
                       lastBlockUtxoRoot: AvlTreeData,
                       boxesToSpend: IndexedSeq[ErgoBox],
                       spendingTransaction: ErgoTransactionTemplate[_ <: InputTemplate],
                       self: ErgoBox,
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[ErgoContext] {
  override def withExtension(newExtension: ContextExtension): ErgoContext = this.copy(extension = newExtension)
}

object ErgoContext {
  type Height = Long

  def dummy(selfDesc: ErgoBox) = ErgoContext(currentHeight = 0,
    lastBlockUtxoRoot = AvlTreeData.dummy, boxesToSpend = IndexedSeq(),
                          spendingTransaction = null, self = selfDesc)

  def fromTransaction(tx: ErgoTransaction,
                      blockchainState: BlockchainState,
                      boxesReader: ErgoBoxReader,
                      inputIndex: Int): Try[ErgoContext] = Try {

    val boxes = tx.inputs.map(_.boxId).map(id => boxesReader.byId(id).get)

    val proverExtension = tx.inputs(inputIndex).spendingProof.extension

    ErgoContext(blockchainState.currentHeight,
                blockchainState.lastBlockUtxoRoot,
                boxes,
                tx,
                boxes(inputIndex),
                proverExtension)
  }
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueInt {
  override val opCode: OpCode = OpCodes.HeightCode

  override def cost[C <: Context[C]](context: C): Int = 2 * Cost.IntConstantDeclaration
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.InputsCode

  override def cost[C <: Context[C]](context: C) =
    context.asInstanceOf[ErgoContext].boxesToSpend.size * Cost.BoxConstant

  val tpe = SCollection(SBox)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.OutputsCode

  override def cost[C <: Context[C]](context: C) =
    context.asInstanceOf[ErgoContext].spendingTransaction.outputs.size * Cost.BoxConstant

  val tpe = SCollection(SBox)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree {
  override val opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode

  override def cost[C <: Context[C]](context: C) = Cost.AvlTreeConstant + 1
}


/** When interpreted evaluates to a BoxConstant built from Context.self */
case object Self extends NotReadyValueBox {
  override val opCode: OpCode = OpCodes.SelfCode

  override def cost[C <: Context[C]](context: C) = Cost.BoxConstant
}
