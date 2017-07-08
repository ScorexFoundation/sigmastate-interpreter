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

trait UtxoVariable[V <: Value] extends Variable[V]

case object SelfHeight extends UtxoVariable[IntLeaf]

case object SelfAmount extends UtxoVariable[IntLeaf]

case object SelfScript extends UtxoVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf]

case object OutputScript extends Variable[PropLeaf]

//todo: more strict-type solution Variable[V] => Value[V]
case class ScopedBinding(bindings: Map[Variable[_], Value], relations: Seq[Relation]) extends StateTree

trait Function extends StateTree

//todo: make a variant with output index
case class TxHasOutput(relation: Relation*) extends Function

case class TxOutput(outIndex: Int, relation: Relation*) extends Function

