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

trait Transformer[IV <: Value, OV <: Value] extends NotReadyValue[OV]{self: OV => }

sealed abstract class Extract[V <: Value] extends Transformer[BoxLeaf, V]{self: V =>
  val box: BoxLeaf
}

case class ExtractHeight(box:BoxLeaf) extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override def cost: Int = 10

  override type M = this.type
}

case class ExtractAmount(box:BoxLeaf) extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override def cost: Int = 10

  override type M = this.type
}

case class ExtractScript(box:BoxLeaf) extends Extract[PropLeaf] with NotReadyValueProp {
  override def cost: Int = 10

  override type M = this.type
}

case class ExtractBytes(box:BoxLeaf) extends Extract[ByteArrayLeaf] with NotReadyValueByteArray {
  override def cost: Int = 10

  override type M = this.type
}

abstract class ExtractRegisterAs[V <: Value] extends Extract[V]{self: V =>
  val registerId: RegisterIdentifier

  override def cost: Int = 10

  override type M = this.type
}

case class ExtractRegisterAsIntLeaf(box: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[IntLeaf] with NotReadyValueIntLeaf

case class ExtractRegisterAsBooleanLeaf(box: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[IntLeaf] with NotReadyValueIntLeaf

case class ExtractRegisterAsByteArrayLeaf(box: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[IntLeaf] with NotReadyValueIntLeaf

case class ExtractRegisterAsPropLeaf(box: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[IntLeaf] with NotReadyValueIntLeaf





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

case object OutputAmount extends NotReadyValueIntLeaf {
  override val cost: Int = Cost.OutputAmount
}

case object OutputScript extends NotReadyValueProp {
  override val cost: Int = Cost.OutputScript
}

case object TxOutBytes extends NotReadyValueByteArray {
  override val cost: Int = Cost.TxOutBytes
}