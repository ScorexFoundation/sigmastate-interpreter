package sigmastate.utxo

import sigmastate._
import Values._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.ErgoContext.Height

case class ErgoContext(currentHeight: Height,
                       lastBlockUtxoRoot: AvlTreeData,
                       boxesToSpend: IndexedSeq[ErgoBox],
                       spendingTransaction: ErgoTransaction,
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

}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueInt {
  override lazy val cost: Int = Cost.HeightAccess
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] {
  val cost = 1
  val tpe = SCollection(SBox)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] {
  val cost = 1
  val tpe = SCollection(SBox)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree


/** When interpreted evaluates to a BoxConstant built from Context.self */
case object Self extends NotReadyValueBox {
  override def cost: Int = 10
}
