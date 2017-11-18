package sigmastate.utxo

import sigmastate.{NotReadyValueIntLeaf, _}
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


trait Transformer[IV <: Value, OV <: Value] extends NotReadyValue[OV]{self: OV =>
  def function(input: EvaluatedValue[IV]): OV
}

case class MapCollection[IV <: Value, OV <: Value](input: CollectionLeaf[IV], mapper: Transformer[IV, OV])
  extends Transformer[CollectionLeaf[IV], CollectionLeaf[OV]] with CollectionLeaf[OV] {self: CollectionLeaf[OV] =>

  override def function(input: EvaluatedValue[CollectionLeaf[IV]]): CollectionLeaf[OV] = ???

  override def cost: Int = ???

  override type M = this.type
}

trait Fold[IV <: Value] extends NotReadyValue[IV] {self: IV =>
  val input: CollectionLeaf[IV]
  val folder: (IV, IV) => IV
  val zero: IV
}

case class Sum(override val input: CollectionLeaf[IntLeaf]) extends Fold[IntLeaf] with NotReadyValueIntLeaf {
  val folder = {case (s, i) =>
    (s, i) match {
      case (si: IntLeafConstant, ii: IntLeafConstant) => IntLeafConstant(si.value + ii.value)
      case _ => UnknownIntLeaf
    }
  }: (IntLeaf, IntLeaf) => IntLeaf
  val zero = IntLeafConstant(0)
}

sealed abstract class Extract[V <: Value] extends Transformer[BoxLeaf, V]{self: V =>
  override def function(box: EvaluatedValue[BoxLeaf]): V
}

sealed trait ExtractHeight extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): IntLeaf = IntLeafConstant(box.value.metadata.creationHeight)
}

case class ExtractHeightInst(box: BoxLeaf) extends ExtractHeight
case object ExtractHeightFn extends ExtractHeight

sealed trait ExtractAmount extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): IntLeaf = IntLeafConstant(box.value.box.value)
}

case class ExtractAmountInst(box: BoxLeaf) extends ExtractAmount
case object ExtractAmountFn extends ExtractAmount


sealed trait ExtractScript extends Extract[PropLeaf] with NotReadyValueProp {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): PropLeaf = PropLeafConstant(box.value.box.proposition)
}

case class ExtractScriptInst(box: BoxLeaf) extends ExtractScript


sealed trait ExtractBytes extends Extract[ByteArrayLeaf] with NotReadyValueByteArray {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): ByteArrayLeaf = ByteArrayLeafConstant(box.value.box.bytes)
}

case class ExtractBytesInst(box: EvaluatedValue[BoxLeaf]) extends ExtractBytes


abstract class ExtractRegisterAs[V <: Value] extends Extract[V]{self: V =>
  val registerId: RegisterIdentifier

  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): V = box.value.box.get(registerId).get.asInstanceOf[V]
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

case object Self extends NotReadyValueBoxLeaf {
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