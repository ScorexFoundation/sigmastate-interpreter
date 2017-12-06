package sigmastate.utxo

import sigmastate.{NotReadyValueIntLeaf, _}
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}


trait Transformer[IV <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>
  val abstractTransformation = true

  def function(input: EvaluatedValue[IV]): OV

  def instantiate(input: IV): TransformerInstantiation[IV, OV]
}

trait TransformerInstantiation[IV <: Value, OV <: Value] extends Transformer[IV, OV] {
  self: OV =>
  val input: IV

  def transformationReady: Boolean = input.evaluated

  override val abstractTransformation = false

  override def instantiate(input: IV): TransformerInstantiation[IV, OV] with OV = this

  def evaluate(): OV = input match {
    case ev: EvaluatedValue[IV] => function(ev)
    case _: NotReadyValue[OV] => this
  }
}

case class MapCollection[IV <: Value, OV <: Value](input: CollectionLeaf[IV],
                                                   mapper: Transformer[IV, OV])
  extends TransformerInstantiation[CollectionLeaf[IV], CollectionLeaf[OV]] with CollectionLeaf[OV] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  //todo: it will fail on FakeBoolean(SigmaTree) instances, the same problem for other similar places
  override def function(cl: EvaluatedValue[CollectionLeaf[IV]]): CollectionLeaf[OV] =
    ConcreteCollection(cl.value.map(el => mapper.function(el.asInstanceOf[EvaluatedValue[IV]])))

  override def cost: Int = 1

  override type M = this.type
}

case class Exists[IV <: Value](input: CollectionLeaf[IV],
                               id: Byte,
                               relations: Relation[_ <: Value, _ <: Value]*)
  extends TransformerInstantiation[CollectionLeaf[IV], BooleanLeaf] with NotReadyValueBoolean {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[CollectionLeaf[IV]]): BooleanLeaf = {
    def rl(arg: IV) = everywherebu(rule[Value] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    OR(input.value.map(el => rl(el)(AND(relations)).get.asInstanceOf[BooleanLeaf]))
  }

  override type M = this.type
}

case class ForAll[IV <: Value](input: CollectionLeaf[IV], relations: Relation[_ <: Value, _ <: Value]*)
  extends TransformerInstantiation[CollectionLeaf[IV], BooleanLeaf] with NotReadyValueBoolean {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[CollectionLeaf[IV]]): BooleanLeaf = {
    def rl(arg: IV) = everywherebu(rule[Transformer[IV, _ <: Value]] {
      case t: Transformer[IV, _] => t.instantiate(arg)
    })

    AND(input.value.map(el => rl(el)(AND(relations)).get.asInstanceOf[BooleanLeaf]))
  }

  override type M = this.type
}

/*
todo: implement

object ByIndex
object Append
object Slice
*/


trait Fold[IV <: Value] extends TransformerInstantiation[CollectionLeaf[IV], IV] with NotReadyValue[IV] {
  self: IV =>
  val input: CollectionLeaf[IV]
  val folder: (IV, IV) => IV
  val zero: IV

  override lazy val cost = (input match {
    case c: EvaluatedValue[CollectionLeaf[IV]] => c.value.map(_.cost).sum
    case _ => 10
  }) + zero.cost
}

case class Sum(override val input: CollectionLeaf[IntLeaf]) extends Fold[IntLeaf] with NotReadyValueIntLeaf {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IntLeaf]].value.forall(_.evaluated)

  val folder = {
    case (s, i) =>
      (s, i) match {
        case (si: IntLeafConstant, ii: IntLeafConstant) => IntLeafConstant(si.value + ii.value)
        case _ => UnknownIntLeaf
      }
  }: (IntLeaf, IntLeaf) => IntLeaf
  val zero = IntLeafConstant(0)

  override def function(input: EvaluatedValue[CollectionLeaf[IntLeaf]]): IntLeaf = {
    input.value.foldLeft(zero: IntLeaf) {case (s: IntLeaf, i: IntLeaf) =>
      folder(s, i): IntLeaf
    }
  }

  override type M = this.type
}

case class SumBytes(override val input: CollectionLeaf[ByteArrayLeaf],
                    zero: ByteArrayLeaf) extends Fold[ByteArrayLeaf] with NotReadyValueByteArray {

  override def transformationReady: Boolean =
    input.evaluated &&
      input.asInstanceOf[ConcreteCollection[ByteArrayLeaf]].value.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[ByteArrayLeaf]]


  val folder = {
    case (s, i) =>
      (s, i) match {
        case (si: ByteArrayLeafConstant, ii: ByteArrayLeafConstant) => ByteArrayLeafConstant(si.value ++ ii.value)
        case _ => UnknownByteArrayLeaf
      }
  }: (ByteArrayLeaf, ByteArrayLeaf) => ByteArrayLeaf

  override def function(input: EvaluatedValue[CollectionLeaf[ByteArrayLeaf]]): ByteArrayLeaf = {
    input.value.foldLeft(zero: ByteArrayLeaf) {case (s: ByteArrayLeaf, i: ByteArrayLeaf) =>
      folder(s, i): ByteArrayLeaf
    }
  }

  override type M = this.type
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

  override def function(box: EvaluatedValue[BoxLeaf]): PropLeaf = {
    PropLeafConstant(box.value)
  }
}

case class ExtractScriptInst(input: BoxLeaf) extends ExtractScript with TransformerInstantiation[BoxLeaf, PropLeaf]

case object ExtractScriptFn extends ExtractScript {
  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, PropLeaf] = ExtractScriptInst(input)
}


sealed trait ExtractBytes extends Extract[ByteArrayLeaf] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

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

  override def function(box: EvaluatedValue[BoxLeaf]): V =
    box.value.box.get(registerId).get.asInstanceOf[V]
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


case class ExtractRegisterAsAvlTreeLeafInst(input: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[AvlTreeLeaf] with TransformerInstantiation[BoxLeaf, AvlTreeLeaf] with NotReadyValueAvlTree

case class ExtractRegisterAsAvlTree(registerId: RegisterIdentifier)
  extends ExtractRegisterAs[AvlTreeLeaf] with NotReadyValueAvlTree {

  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, AvlTreeLeaf] =
    ExtractRegisterAsAvlTreeLeafInst(input, registerId)
}


case class ExtractRegisterAsGroupElementInst(input: BoxLeaf, registerId: RegisterIdentifier)
  extends ExtractRegisterAs[GroupElementLeaf]
    with TransformerInstantiation[BoxLeaf, GroupElementLeaf]
    with NotReadyValueGroupElement

case class ExtractRegisterAsGroupElement(registerId: RegisterIdentifier)
  extends ExtractRegisterAs[GroupElementLeaf] with NotReadyValueGroupElement {

  override def instantiate(input: BoxLeaf): TransformerInstantiation[BoxLeaf, GroupElementLeaf] =
    ExtractRegisterAsGroupElementInst(input, registerId)
}

//todo: extract as box leaf