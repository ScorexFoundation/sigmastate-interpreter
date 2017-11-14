package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier
import sigmastate.utxo.UtxoContext.Height

case class BoxMetadata(creationHeight: Height, boxIndex: Short)

case class BoxWithMetadata(box: SigmaStateBox, metadata: BoxMetadata)

case class UtxoContext(currentHeight: Height,
                       boxesToSpend: Seq[BoxWithMetadata],
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

trait Transformer[IV <: Value, +OV <: Value] extends Variable[OV]

sealed abstract class Extract[+V <: Value] extends Transformer[BoxLeaf, V]

case object ExtractHeight extends Extract[NonNegativeIntLeaf] {
  override def cost: Int = 10

  override type M = this.type
}

case object ExtractAmount extends Extract[NonNegativeIntLeaf] {
  override def cost: Int = 10

  override type M = this.type
}

case object ExtractScript extends Extract[PropLeaf] {
  override def cost: Int = 10

  override type M = this.type
}

case object ExtractBytes extends Extract[ByteArrayLeaf] {
  override def cost: Int = 10

  override type M = this.type
}

case class ExtractRegister[V <: Value](registerId: RegisterIdentifier) extends Extract[V] {
  override def cost: Int = 10

  override type M = this.type
}

case class RunExtract[V <: Value, E <: Extract[V]](operand: BoxLeaf, extractor: E)
  extends OneArgumentOperation with Transformer[BoxLeaf, V] {
  override def cost: Int = 10

  override type M = this.type
}

case class Collection[V <: Value](values: Seq[V]) extends Value {
  override def cost: Int = values.map(_.cost).sum

  override type M = this.type
}

//todo: inheritance?
case class MapCollection[IV <: Value](collection: Collection[IV], mapper: OneArgumentOperation) extends Value {

  override def cost: Int = 10

  override type M = this.type
}

object Inputs extends Collection[BoxLeaf](values = null)
object Outputs extends Collection[BoxLeaf](values = null)

case class Exists[V <: Value](collection: Collection[V], relation: Relation) extends Transformer[V, BooleanLeaf] {

  override def cost: Int = 10

  override type M = this.type
}

/*
todo: implement

object Forall
object FoldLeft
object Append
object Slice
object ByIndex
*/

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