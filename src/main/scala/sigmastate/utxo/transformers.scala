package sigmastate.utxo

import sigmastate.{NotReadyValueIntLeaf, _}
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}


trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {

  val input: Value[IV]

  def transformationReady: Boolean = input.evaluated

  def function(input: EvaluatedValue[IV]): Value[OV]

  def function(): Value[OV] = input match{
    case ev: EvaluatedValue[IV] => function(ev)
    case _ => ???
  }

  def evaluate(): Value[OV] = input match {
    case ev: EvaluatedValue[IV] => function(ev)
    case _: NotReadyValue[OV] => this
  }
}


case class MapCollection[IV <: SType, OV <: SType](input: CollectionLeaf[IV],
                                                   id: Byte,
                                                   mapper: Transformer[IV, OV])
  extends Transformer[SCollection[IV], SCollection[OV]] with CollectionLeaf[OV] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)


  //todo: it will fail on FakeBoolean(SigmaTree) instances, the same problem for other similar places
  override def function(cl: EvaluatedValue[SCollection[IV]]): CollectionLeaf[OV] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    ConcreteCollection(cl.value.map(el => (rl(el)(mapper)).get.asInstanceOf[Transformer[IV, OV]]).map(_.function()))
  }

  override def cost: Int = 1

  override type M = this.type
}

case class Exists[IV <: SType](input: CollectionLeaf[IV],
                               id: Byte,
                               relations: Relation[_, _]*)
  extends Transformer[SCollection[IV], SBoolean.type] with NotReadyValueBoolean {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[SCollection[IV]]): BooleanLeaf = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    OR(input.value.map(el => rl(el)(AND(relations)).get.asInstanceOf[BooleanLeaf]))
  }

  override type M = this.type
}

case class ForAll[IV <: SType](input: CollectionLeaf[IV],
                               id: Byte,
                               relations: Relation[_, _]*)
  extends Transformer[SCollection[IV], SBoolean.type] with NotReadyValueBoolean {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[SCollection[IV]]): BooleanLeaf = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
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


case class Fold[IV <: SType](input: CollectionLeaf[IV],
                             id: Byte,
                             zero: Value[IV],
                             accId: Byte,
                             foldOp: TwoArgumentsOperation[IV, IV, IV])
  extends Transformer[SCollection[IV], IV] with NotReadyValue[IV] {


  override def transformationReady: Boolean =
    input.evaluated &&
      input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[IV]]


  override lazy val cost: Int = (input match {
    case c: EvaluatedValue[SCollection[IV]] => c.value.map(_.cost).sum
    case _ => 10
  }) + zero.cost

  override def function(input: EvaluatedValue[SCollection[IV]]): Value[IV] = {
    def rl(arg: Value[IV], acc: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
      case t: TaggedVariable[IV] if t.id == accId => acc
    })

    input.value.foldLeft(zero) {case (acc: Value[IV], elem: Value[IV]) =>
      rl(elem, acc)(foldOp).get.asInstanceOf[Value[IV]]
    }
  }
}

object Fold {
  def sum(input: CollectionLeaf[SInt.type]) =
    Fold(input, 21, IntLeafConstant(0), 22, Plus(TaggedInt(22), TaggedInt(21)))

  def sumBytes(input: CollectionLeaf[SByteArray.type]) =
    Fold[SByteArray.type](input, 21, EmptyByteArray, 22, Append(TaggedByteArray(22), TaggedByteArray(21)))
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

sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
  override def function(box: EvaluatedValue[SBox.type]): Value[V]
}

case class ExtractHeight(input: BoxLeaf) extends Extract[SInt.type] with NotReadyValueIntLeaf {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[SBox.type]): IntLeaf =
    IntLeafConstant(box.value.metadata.creationHeight)
}


case class ExtractAmount(input: BoxLeaf) extends Extract[SInt.type] with NotReadyValueIntLeaf {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[SBox.type]): IntLeaf = IntLeafConstant(box.value.box.value)
}


case class ExtractScript(input: BoxLeaf) extends Extract[SProp.type] with NotReadyValueProp {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[SBox.type]): PropLeaf = {
    PropLeafConstant(box.value)
  }
}


case class ExtractBytes(input: BoxLeaf) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[SBox.type]): ByteArrayLeaf = ByteArrayLeafConstant(box.value.box.bytes)
}


abstract class ExtractRegisterAs[V <: SType] extends Extract[V] {
  val registerId: RegisterIdentifier
  val default: Option[Value[V]]

  override def cost: Int = 10

  override type M = this.type

  override def function(box: EvaluatedValue[SBox.type]): Value[V] =
    box.value.box.get(registerId).orElse(default).get.asInstanceOf[Value[V]]
}

case class ExtractRegisterAsIntLeaf(input: BoxLeaf,
                                    registerId: RegisterIdentifier,
                                    default: Option[IntLeaf] = None)
  extends ExtractRegisterAs[SInt.type] with Transformer[SBox.type, SInt.type] with NotReadyValueIntLeaf


case class ExtractRegisterAsBooleanLeaf(input: BoxLeaf,
                                        registerId: RegisterIdentifier,
                                        default: Option[BooleanLeaf] = None)
  extends ExtractRegisterAs[SBoolean.type] with Transformer[SBox.type, SBoolean.type] with NotReadyValueBoolean


case class ExtractRegisterAsByteArrayLeaf(input: BoxLeaf,
                                          registerId: RegisterIdentifier,
                                          default: Option[ByteArrayLeaf] = None)
  extends ExtractRegisterAs[SByteArray.type] with Transformer[SBox.type, SByteArray.type] with NotReadyValueByteArray


case class ExtractRegisterAsPropLeaf(input: BoxLeaf,
                                     registerId: RegisterIdentifier,
                                     default: Option[PropLeaf] = None)
  extends ExtractRegisterAs[SProp.type] with Transformer[SBox.type, SProp.type] with NotReadyValueProp


case class ExtractRegisterAsAvlTreeLeaf(input: BoxLeaf,
                                        registerId: RegisterIdentifier,
                                        default: Option[AvlTreeLeaf] = None)
  extends ExtractRegisterAs[SAvlTree.type] with Transformer[SBox.type, SAvlTree.type] with NotReadyValueAvlTree


case class ExtractRegisterAsGroupElement(input: BoxLeaf,
                                         registerId: RegisterIdentifier,
                                         default: Option[GroupElementLeaf] = None)
  extends ExtractRegisterAs[SGroupElement.type]
    with Transformer[SBox.type, SGroupElement.type]
    with NotReadyValueGroupElement


//todo: extract as a box leaf