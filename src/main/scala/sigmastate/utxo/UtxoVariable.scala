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

trait UtxoVariable[V <: Value] extends Variable[V] {
  override def cost: Int = Cost.SelfVariableDeclaration //todo: imprecise, especially for SelfScipt
}

case object SelfHeight extends UtxoVariable[IntLeaf]

case object SelfAmount extends UtxoVariable[IntLeaf]

case object SelfScript extends UtxoVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf] {
  override def cost: Int = Cost.OutputAmount
}

case object OutputScript extends Variable[PropLeaf] {
  override def cost: Int = Cost.OutputScript
}


trait Function extends StateTree

case class TxHasOutput(relation: Relation*) extends Function {
  override def cost: Int = relation.length + Cost.TxHasOutputDeclaration
}

case class TxOutput(outIndex: Int, relation: Relation*) extends Function {
  override def cost: Int = relation.length + Cost.TxOutputDeclaration
}