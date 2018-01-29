package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.UtxoContext.Height

case class UtxoContext(currentHeight: Height,
                       lastBlockUtxoRoot: AvlTreeData,
                       boxesToSpend: IndexedSeq[BoxWithMetadata],
                       spendingTransaction: SigmaStateTransaction,
                       self: BoxWithMetadata,
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[UtxoContext] {
  override def withExtension(newExtension: ContextExtension): UtxoContext = this.copy(extension = newExtension)
}

object UtxoContext {
  type Height = Long

  def dummy(selfDesc: BoxWithMetadata) = UtxoContext(currentHeight = 0, lastBlockUtxoRoot = AvlTreeData.dummy, boxesToSpend = IndexedSeq(),
                          spendingTransaction = null, self = selfDesc)

}

case object Height extends NotReadyValueInt {
  override lazy val cost: Int = Cost.HeightAccess
}

case object Inputs extends LazyCollection[SBox.type] {
  val cost = 1
}

case object Outputs extends LazyCollection[SBox.type] {
  val cost = 1
}

case object LastBlockUtxoRootHash extends NotReadyValueAvlTree


case object Self extends NotReadyValueBox {
  override def cost: Int = 10

  override type M = this.type
}
