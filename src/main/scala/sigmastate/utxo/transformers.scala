package sigmastate.utxo

import sigmastate.{NotReadyValueIntLeaf, _}
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}


trait Transformer[IV <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>

  val input: IV

  def transformationReady: Boolean = input.evaluated

  def function(input: EvaluatedValue[IV]): OV

  def function(): OV = input match{
    case ev: EvaluatedValue[IV] => function(ev)
    case _ => ???
  }

  def evaluate(): OV = input match {
    case ev: EvaluatedValue[IV] => function(ev)
    case _: NotReadyValue[OV] => this
  }
}


case class MapCollection[IV <: Value, OV <: Value](input: CollectionLeaf[IV],
                                                   id: Byte,
                                                   mapper: Transformer[IV, OV])
  extends Transformer[CollectionLeaf[IV], CollectionLeaf[OV]] with CollectionLeaf[OV] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)


  //todo: it will fail on FakeBoolean(SigmaTree) instances, the same problem for other similar places
  override def function(cl: EvaluatedValue[CollectionLeaf[IV]]): CollectionLeaf[OV] = {
    def rl(arg: IV) = everywherebu(rule[Value] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    ConcreteCollection(cl.value.map(el => (rl(el)(mapper)).get.asInstanceOf[Transformer[IV, OV]]).map(_.function()))
  }

  override def cost: Int = 1

  override type M = this.type
}

case class Exists[IV <: Value](input: CollectionLeaf[IV],
                               id: Byte,
                               relations: Relation[_ <: Value, _ <: Value]*)
  extends Transformer[CollectionLeaf[IV], BooleanLeaf] with NotReadyValueBoolean {

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

case class ForAll[IV <: Value](input: CollectionLeaf[IV],
                               id: Byte,
                               relations: Relation[_ <: Value, _ <: Value]*)
  extends Transformer[CollectionLeaf[IV], BooleanLeaf] with NotReadyValueBoolean {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[CollectionLeaf[IV]]): BooleanLeaf = {
    def rl(arg: IV) = everywherebu(rule[Value] {
      case t: TaggedVariable[IV] if t.id == id => arg
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


abstract class Fold[IV <: Value](input: CollectionLeaf[IV],
                             id: Byte,
                             zero: IV,
                             accId: Byte,
                             foldOp: TwoArgumentsOperation[IV, IV, IV])
  extends Transformer[CollectionLeaf[IV], IV] with NotReadyValue[IV] {

  self: IV =>

  override def transformationReady: Boolean =
    input.evaluated &&
      input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[IV]]


  override lazy val cost: Int = (input match {
    case c: EvaluatedValue[CollectionLeaf[IV]] => c.value.map(_.cost).sum
    case _ => 10
  }) + zero.cost

  override def function(input: EvaluatedValue[CollectionLeaf[IV]]): IV = {
    def rl(arg: IV, acc: IV) = everywherebu(rule[Value] {
      case t: TaggedVariable[IV] if t.id == id => arg
      case t: TaggedVariable[IV] if t.id == accId => arg
    })

    input.value.foldLeft(zero) {case (acc: IV, elem: IV) =>
      rl(elem, acc)(foldOp).get.asInstanceOf[IV]
    }
  }
}

object Fold {
  def sum(input: CollectionLeaf[IntLeaf]) = Fold(input, 21, IntLeafConstant(0), 22, Plus(TaggedInt(22), TaggedInt(21)))

  def sumBytes(input: CollectionLeaf[ByteArrayLeaf]) =
    Fold(input, 21, EmptyByteArray, 22, Append(TaggedByteArray(22), TaggedByteArray(21)))
}

/*
case class Sum(override val input: CollectionLeaf[IntLeaf]) extends Fold[IntLeaf] with NotReadyValueIntLeaf {

  val folder = {
    case (s, i) =>
      (s, i) match {
        case (si: IntLeafConstant, ii: IntLeafConstant) => IntLeafConstant(si.value + ii.value)
        case _ => UnknownIntLeaf
      }
  }: (IntLeaf, IntLeaf) => IntLeaf
  val zero = IntLeafConstant(0)

  override type M = this.type
}

case class SumBytes(override val input: CollectionLeaf[ByteArrayLeaf],
                    zero: ByteArrayLeaf) extends Fold[ByteArrayLeaf] with NotReadyValueByteArray {

  val folder = {
    case (s, i) =>
      (s, i) match {
        case (si: ByteArrayLeafConstant, ii: ByteArrayLeafConstant) => ByteArrayLeafConstant(si.value ++ ii.value)
        case _ => UnknownByteArrayLeaf
      }
  }: (ByteArrayLeaf, ByteArrayLeaf) => ByteArrayLeaf

  override type M = this.type
}*/

sealed abstract class Extract[V <: Value] extends Transformer[BoxLeaf, V] {
  self: V =>
  override def function(box: EvaluatedValue[BoxLeaf]): V

}

case class ExtractHeight(input: BoxLeaf) extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): IntLeaf = IntLeafConstant(box.value.metadata.creationHeight)
}


case class ExtractAmount(input: BoxLeaf) extends Extract[IntLeaf] with NotReadyValueIntLeaf {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): IntLeaf = IntLeafConstant(box.value.box.value)
}


case class ExtractScript(input: BoxLeaf) extends Extract[PropLeaf] with NotReadyValueProp {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): PropLeaf = {
    PropLeafConstant(box.value)
  }
}


case class ExtractBytes(input: BoxLeaf) extends Extract[ByteArrayLeaf] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): ByteArrayLeaf = ByteArrayLeafConstant(box.value.box.bytes)
}


abstract class ExtractRegisterAs[V <: Value] extends Extract[V] {
  self: V =>

  val registerId: RegisterIdentifier
  val default: Option[V]

  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[BoxLeaf]): V =
    box.value.box.get(registerId).orElse(default).get.asInstanceOf[V]
}

case class ExtractRegisterAsIntLeaf(input: BoxLeaf,
                                    registerId: RegisterIdentifier,
                                    default: Option[IntLeaf] = None)
  extends ExtractRegisterAs[IntLeaf] with Transformer[BoxLeaf, IntLeaf] with NotReadyValueIntLeaf


case class ExtractRegisterAsBooleanLeaf(input: BoxLeaf,
                                        registerId: RegisterIdentifier,
                                        default: Option[BooleanLeaf] = None)
  extends ExtractRegisterAs[BooleanLeaf] with Transformer[BoxLeaf, BooleanLeaf] with NotReadyValueBoolean


case class ExtractRegisterAsByteArrayLeaf(input: BoxLeaf,
                                          registerId: RegisterIdentifier,
                                          default: Option[ByteArrayLeaf] = None)
  extends ExtractRegisterAs[ByteArrayLeaf] with Transformer[BoxLeaf, ByteArrayLeaf] with NotReadyValueByteArray


case class ExtractRegisterAsPropLeaf(input: BoxLeaf,
                                     registerId: RegisterIdentifier,
                                     default: Option[PropLeaf] = None)
  extends ExtractRegisterAs[PropLeaf] with Transformer[BoxLeaf, PropLeaf] with NotReadyValueProp


case class ExtractRegisterAsAvlTreeLeaf(input: BoxLeaf,
                                        registerId: RegisterIdentifier,
                                        default: Option[AvlTreeLeaf] = None)
  extends ExtractRegisterAs[AvlTreeLeaf] with Transformer[BoxLeaf, AvlTreeLeaf] with NotReadyValueAvlTree


case class ExtractRegisterAsGroupElement(input: BoxLeaf,
                                         registerId: RegisterIdentifier,
                                         default: Option[GroupElementLeaf] = None)
  extends ExtractRegisterAs[GroupElementLeaf] with Transformer[BoxLeaf, GroupElementLeaf] with NotReadyValueGroupElement


//todo: extract as a box leaf