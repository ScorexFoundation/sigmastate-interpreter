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


trait Transformer[IV <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>
  val abstractTransformation = false

  def function(input: EvaluatedValue[IV]): OV

  def instantiate(input: IV): TransformerInstantiation[IV, OV]
}

trait TransformerInstantiation[IV <: Value, OV <: Value] extends Transformer[IV, OV] {
  self: OV =>
  val input: IV

  override val abstractTransformation = true

  override def instantiate(input: IV) = this

  def evaluate(): OV = input match {
    case ev: EvaluatedValue[IV] => function(ev)
    case _: NotReadyValue[OV] => this
  }
}

case class MapCollection[IV <: Value, OV <: Value](input: CollectionLeaf[IV], mapper: Transformer[IV, OV])
  extends TransformerInstantiation[CollectionLeaf[IV], CollectionLeaf[OV]] with CollectionLeaf[OV] {

  override def function(cl: EvaluatedValue[CollectionLeaf[IV]]): CollectionLeaf[OV] =
    ConcreteCollection(cl.value.map(el => mapper.function(el.asInstanceOf[EvaluatedValue[IV]])))

  override def cost: Int = 1

  override type M = this.type
}

case class Exists[IV <: Value](input: CollectionLeaf[IV], relations: Relation[_ <: Value, _ <: Value]*)
  extends TransformerInstantiation[CollectionLeaf[IV], BooleanLeaf] with NotReadyValueBoolean {

  override val cost = input.cost + relations.size

  override def function(input: EvaluatedValue[CollectionLeaf[IV]]): BooleanLeaf =
    OR(input.value.map { el =>
      AND(relations.map { r =>
        r
      })
    })

  override type M = this.type
}

//Exists(Outputs, GT(ExtractAmountFn, IntLeafConstant(10)))


trait Fold[IV <: Value] extends NotReadyValue[IV] {
  self: IV =>
  val input: CollectionLeaf[IV]
  val folder: (IV, IV) => IV
  val zero: IV
}

case class Sum(override val input: CollectionLeaf[IntLeaf]) extends Fold[IntLeaf] with NotReadyValueIntLeaf {
  val folder = {
    case (s, i) =>
      (s, i) match {
        case (si: IntLeafConstant, ii: IntLeafConstant) => IntLeafConstant(si.value + ii.value)
        case _ => UnknownIntLeaf
      }
  }: (IntLeaf, IntLeaf) => IntLeaf
  val zero = IntLeafConstant(0)
}

sealed abstract class Extract[V <: Value] extends Transformer[BoxLeaf, V] {
  self: V =>
  override def function(box: EvaluatedValue[BoxLeaf]): V

}

sealed trait ExtractHeight extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): IntLeaf = IntLeafConstant(box.value.metadata.creationHeight)
}

case class ExtractHeightInst(input: BoxLeaf) extends ExtractHeight with TransformerInstantiation[BoxLeaf, IntLeaf]

case object ExtractHeightFn extends ExtractHeight {
  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, IntLeaf] = ExtractHeightInst(input)
}


sealed trait ExtractAmount extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): IntLeaf = IntLeafConstant(box.value.box.value)
}

case class ExtractAmountInst(input: BoxLeaf) extends ExtractAmount with TransformerInstantiation[BoxLeaf, IntLeaf]

case object ExtractAmountFn extends ExtractAmount {
  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, IntLeaf] = ExtractAmountInst(input)
}


sealed trait ExtractScript extends Extract[PropLeaf] with NotReadyValueProp {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): PropLeaf = PropLeafConstant(box.value.box.proposition)
}

case class ExtractScriptInst(input: BoxLeaf) extends ExtractScript with TransformerInstantiation[BoxLeaf, PropLeaf]

case object ExtractScriptFn extends ExtractScript {
  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, PropLeaf] = ExtractScriptInst(input)
}


sealed trait ExtractBytes extends Extract[ByteArrayLeaf] with NotReadyValueByteArray {
  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): ByteArrayLeaf = ByteArrayLeafConstant(box.value.box.bytes)
}

case class ExtractBytesInst(input: BoxLeaf)
  extends ExtractBytes with TransformerInstantiation[BoxLeaf, ByteArrayLeaf]

case object ExtractBytesFn extends ExtractBytes {
  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, ByteArrayLeaf] = ExtractBytesInst(input)
}


abstract class ExtractRegisterAs[V <: Value] extends Extract[V] {
  self: V =>
  val registerId: RegisterIdentifier

  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): V = box.value.box.get(registerId).get.asInstanceOf[V]
}

case class ExtractRegisterAsIntLeafInst(input: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[IntLeaf] with TransformerInstantiation[BoxLeaf, IntLeaf] with NotReadyValueIntLeaf

case class ExtractRegisterAsIntLeaf(registerId: RegisterIdentifier)
  extends ExtractRegisterAs[IntLeaf] with NotReadyValueIntLeaf {

  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, IntLeaf] =
    ExtractRegisterAsIntLeafInst(input, registerId)
}


case class ExtractRegisterAsBooleanLeafInst(input: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[BooleanLeaf] with TransformerInstantiation[BoxLeaf, BooleanLeaf] with NotReadyValueBoolean

case class ExtractRegisterAsBooleanLeaf(registerId: RegisterIdentifier)
  extends ExtractRegisterAs[BooleanLeaf] with NotReadyValueBoolean {

  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, BooleanLeaf] =
    ExtractRegisterAsBooleanLeafInst(input, registerId)
}


case class ExtractRegisterAsByteArrayLeafInst(input: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[ByteArrayLeaf] with TransformerInstantiation[BoxLeaf, ByteArrayLeaf] with NotReadyValueByteArray

case class ExtractRegisterAsByteArrayLeaf(registerId: RegisterIdentifier)
  extends ExtractRegisterAs[ByteArrayLeaf] with NotReadyValueByteArray {

  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, ByteArrayLeaf] =
    ExtractRegisterAsByteArrayLeafInst(input, registerId)
}


case class ExtractRegisterAsPropLeafInst(input: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[PropLeaf] with TransformerInstantiation[BoxLeaf, PropLeaf] with NotReadyValueProp

case class ExtractRegisterAsPropLeaf(registerId: RegisterIdentifier)
  extends ExtractRegisterAs[PropLeaf] with NotReadyValueProp {

  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, PropLeaf] =
    ExtractRegisterAsPropLeafInst(input, registerId)
}

//todo: extract as box leaf


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