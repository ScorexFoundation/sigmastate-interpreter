package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}


case class UtxoContext(currentHeight: Long,
                       spendingTransaction: SigmaStateTransaction,
                       self: (SigmaStateBox, Long),
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[UtxoContext] {
  override def withExtension(newExtension: ContextExtension): UtxoContext = this.copy(extension = newExtension)
}

trait UtxoVariable[V <: Value] extends Variable[V] {
  override val cost: Int = 1 //todo: imprecise, especially for SelfScipr
}

case object SelfHeight extends UtxoVariable[IntLeaf]

case object SelfAmount extends UtxoVariable[IntLeaf]

case object SelfScript extends UtxoVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf]{
  override val cost: Int = 1 //todo: imprecise
}

case object OutputScript extends Variable[PropLeaf]{
  override val cost: Int = 1 //todo: imprecise
}


trait Function extends StateTree

case class TxHasOutput(relation: Relation*) extends Function {
  override val cost: Int = 0  //todo: imprecise
}

case class TxOutput(outIndex: Int, relation: Relation*) extends Function {
  override val cost: Int = relation.map(_.cost).sum //todo: imprecise
}