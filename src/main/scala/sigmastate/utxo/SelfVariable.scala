package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier
import sigmastate.utxo.UtxoContext.Height

case class BoxMetadata(creationHeight: Height, boxIndex: Short)

case class BoxWithMetadata(box: SigmaStateBox, metadata: BoxMetadata)

case class UtxoContext(currentHeight: Height,
                       spendingTransaction: SigmaStateTransaction,
                       self: BoxWithMetadata,
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[UtxoContext] {
  override def withExtension(newExtension: ContextExtension): UtxoContext = this.copy(extension = newExtension)
}

object UtxoContext {
  type Height = Long
}

case class BoxLeaf(value: BoxWithMetadata) extends Value {
  override def cost: Int = 10

  override type M = this.type
}

object BoxField {
  sealed trait Field[+V <: Value]

  object Height extends Field[NonNegativeIntLeaf]
  object Amount extends Field[NonNegativeIntLeaf]
  object Script extends Field[PropLeaf]

  case class Register[V <: Value](registerId: RegisterIdentifier) extends Field[V]
}

case class Extract[V <: Value](box: BoxLeaf, field: BoxField.Field[V]) extends Variable[V] {
  override def cost: Int = 10

  override type M = this.type
}

object Self extends BoxLeaf(null) {

  override def cost: Int = 10

  override type M = this.type
}

/*
trait SelfVariable[V <: Value] extends Variable[V] {
  override def cost: Int = Cost.SelfVariableDeclaration
}

case object SelfHeight extends SelfVariable[NonNegativeIntLeaf]

case object SelfAmount extends SelfVariable[NonNegativeIntLeaf]

case object SelfScript extends SelfVariable[PropLeaf]
*/

case object OutputAmount extends Variable[NonNegativeIntLeaf] {
  override val cost: Int = Cost.OutputAmount
}

case object OutputScript extends Variable[PropLeaf] {
  override val cost: Int = Cost.OutputScript
}

case object TxOutBytes extends Variable[ByteArrayLeaf] {
  override val cost: Int = Cost.TxOutBytes
}