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

trait BoxLeaf extends Value {
  val value: BoxWithMetadata

  override def cost: Int = 10

  override type M = this.type
}

case class BoxLeafInstantiation(override val value: BoxWithMetadata) extends BoxLeaf

object BoxLeaf {
  def apply(b: BoxWithMetadata) = BoxLeafInstantiation(b)
}

object BoxField {

  sealed trait Field[+V <: Value]

  object Height extends Field[NonNegativeIntLeaf]

  object Amount extends Field[NonNegativeIntLeaf]

  object Script extends Field[PropLeaf]

  object Bytes extends Field[ByteArrayLeaf]

  case class Register[V <: Value](registerId: RegisterIdentifier) extends Field[V]
}

trait Transformer[IV <: Value, OV <: Value] extends Variable[OV]

case class Extract[V <: Value](box: BoxLeaf, field: BoxField.Field[V]) extends Transformer[BoxLeaf, V] {
  override def cost: Int = 10

  override type M = this.type
}

/*
case class Collection[V <: Value](values: Seq[V])

case class Map[IV, OV](collection: Collection[IV], mapper: Transformer[IV, OV])
  extends Transformer[Collection[IV], Collection[OV]] {

  override def cost: Int = 10

  override type M = this.type
}*/


case object Self extends BoxLeaf {
  override lazy val value = ???

  override def cost: Int = 10

  override type M = this.type
}

case object OutputAmount extends Variable[NonNegativeIntLeaf] {
  override val cost: Int = Cost.OutputAmount
}

case object OutputScript extends Variable[PropLeaf] {
  override val cost: Int = Cost.OutputScript
}

case object TxOutBytes extends Variable[ByteArrayLeaf] {
  override val cost: Int = Cost.TxOutBytes
}