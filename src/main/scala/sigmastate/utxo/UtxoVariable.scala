package sigmastate.utxo

import sigmastate._


case class UtxoContext(currentHeight: Long,
                       spendingTransaction: SigmaStateTransaction,
                       self: (SigmaStateBox, Long),
                       override val extension: Map[ExtensionRequest.Id, _ <: Triple] = Map()
                      ) extends Context

trait UtxoVariable[V <: Value] extends Variable[V]

case object SelfHeight extends UtxoVariable[IntLeaf]

case object SelfAmount extends UtxoVariable[IntLeaf]

case object SelfScript extends UtxoVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf]

case object OutputScript extends Variable[PropLeaf]

//todo: more strict-type solution Variable[V] => Value[V]
case class ScopedBinding(bindings: Map[Variable[_], Value], relations: Seq[Relation]) extends StateTree

trait Function extends StateTree

case class TxHasOutput(relation: Relation*) extends Function


