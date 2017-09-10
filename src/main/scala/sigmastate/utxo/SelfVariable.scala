package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.CostTable.Cost


case class UtxoContext(currentHeight: Long,
                       spendingTransaction: SigmaStateTransaction,
                       self: (SigmaStateBox, Long),
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[UtxoContext] {
  override def withExtension(newExtension: ContextExtension): UtxoContext = this.copy(extension = newExtension)
}

trait SelfVariable[V <: Value] extends Variable[V] {
  override def cost: Int = Cost.SelfVariableDeclaration
}

case object SelfHeight extends SelfVariable[IntLeaf]

case object SelfAmount extends SelfVariable[IntLeaf]

case object SelfScript extends SelfVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf] {
  override val cost: Int = Cost.OutputAmount
}

case object OutputScript extends Variable[PropLeaf] {
  override val cost: Int = Cost.OutputScript
}

case object TxOutBytes extends Variable[ByteArrayLeaf] {
  override val cost: Int = Cost.TxOutBytes
}


trait Function extends StateTree

case class TxHasOutput(relation: Relation*) extends Function {
  override val cost: Int = relation.length + Cost.TxHasOutputDeclaration
}

case class TxOutput(outIndex: Int, relation: Relation*) extends Function {
  override val cost: Int = relation.length + Cost.TxOutputDeclaration
}