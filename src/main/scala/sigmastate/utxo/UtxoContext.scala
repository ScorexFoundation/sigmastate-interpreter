package sigmastate.utxo

import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.UtxoContext.Height

case class UtxoContext(currentHeight: Height,
                       boxesToSpend: IndexedSeq[BoxWithMetadata],
                       spendingTransaction: SigmaStateTransaction,
                       self: BoxWithMetadata,
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[UtxoContext] {
  override def withExtension(newExtension: ContextExtension): UtxoContext = this.copy(extension = newExtension)
}

object UtxoContext {
  type Height = Long
}