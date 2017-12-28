package sigmastate.utxo

import sigmastate.{AvlTreeData, TrueLeaf}
import sigmastate.interpreter.{Context, ContextExtension}
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