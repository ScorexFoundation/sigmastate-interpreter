package sigmastate.utxo

import sigmastate.{NotReadyValueInt, _}
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}

import scala.reflect.runtime.universe._

trait Transformer[IT, IV <: SType[IT], OT, OV <: SType[OT]] extends NotReadyValue[OT, OV] {

  val input: Value[IT, IV]

  def transformationReady: Boolean = input.evaluated

  def function(input: EvaluatedValue[IT, IV]): Value[OT, OV]

  def function(): Value[OT, OV] = input match {
    case ev: EvaluatedValue[IT, IV] => function(ev)
    case _ => ???
  }

  def evaluate(): Value[OT, OV] = input match {
    case ev: EvaluatedValue[IT, IV] => function(ev)
    case _: NotReadyValue[OT, OV] => this
  }
}


case class MapCollection[IT, IV <: SType[IT], OT, OV <: SType[OT]](input: Value[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]],
                                                                                       id: Byte,
                                                                                       mapper: Transformer[IT, IV, OT, OV])
  extends Transformer[IT, SCollection[IT, IV], OT, SCollection[OT, OV]] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IT, IV]].value.forall(_.evaluated)

  override def function(cl: EvaluatedValue[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]]): Value[IndexedSeq[Value[OT, OV]], SCollection[OT, OV]] = {
    def rl(arg: Value[IT, IV]) = everywherebu(rule[Value[IT, IV]] {
      case t: TaggedVariable[IT, IV] if t.id == id => arg
    })

    ConcreteCollection(cl.value.map(el => (rl(el)(mapper)).get.asInstanceOf[Transformer[IT, IV, OT, OV]]).map(_.function()))
  }

  override def cost: Int = 1
}

case class Exists[IT, IV <: SType[IT]](input: Value[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]],
                                       id: Byte,
                                       relations: Relation[_, _, _, _]*)
  extends Transformer[IndexedSeq[Value[IT, IV]], SCollection[IT, IV], Boolean, SBoolean.type] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IT, IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]]): Value[Boolean, SBoolean.type] = {
    def rl(arg: Value[IT, IV]) = everywherebu(rule[Value[IT, IV]] {
      case t: TaggedVariable[IT, IV] if t.id == id => arg
    })

    OR(input.value.map(el => rl(el)(AND(relations)).get.asInstanceOf[Value[Boolean, SBoolean.type]]))
  }
}

case class ForAll[IT, IV <: SType[IT]](input: Value[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]],
                                       id: Byte,
                                       relations: Relation[_, _, _, _]*)
  extends Transformer[IndexedSeq[Value[IT, IV]], SCollection[IT, IV], Boolean, SBoolean.type] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IT, IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost + relations.size

  //todo: cost
  override def function(input: EvaluatedValue[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]]): Value[Boolean, SBoolean.type] = {
    def rl(arg: Value[IT, IV]) = everywherebu(rule[Value[IT, IV]] {
      case t: TaggedVariable[IT, IV] if t.id == id => arg
    })

    AND(input.value.map(el => rl(el)(AND(relations)).get.asInstanceOf[Value[Boolean, SBoolean.type]]))
  }
}

/*
todo: implement

object Append
object Slice
*/


case class Fold[IT, IV <: SType[IT]](input: Value[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]],
                                     id: Byte,
                                     zero: Value[IT, IV],
                                     accId: Byte,
                                     foldOp: TwoArgumentsOperation[IT, IV, IT, IV, IT, IV])
  extends Transformer[IndexedSeq[Value[IT, IV]], SCollection[IT, IV], IT, IV] with NotReadyValue[IT, IV] {


  override def transformationReady: Boolean =
    input.evaluated &&
      input.asInstanceOf[ConcreteCollection[IT, IV]].value.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[IT, IV]]


  override lazy val cost: Int = (input match {
    case c: EvaluatedValue[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]] => c.value.map(_.cost).sum
    case _ => 10
  }) + zero.cost

  override def function(input: EvaluatedValue[IndexedSeq[Value[IT, IV]], SCollection[IT, IV]]): Value[IT, IV] = {
    def rl(arg: Value[IT, IV], acc: Value[IT, IV]) = everywherebu(rule[Value[IT, IV]] {
      case t: TaggedVariable[IT, IV] if t.id == id => arg
      case t: TaggedVariable[IT, IV] if t.id == accId => acc
    })

    input.value.foldLeft(zero) { case (acc: Value[IT, IV], elem: Value[IT, IV]) =>
      rl(elem, acc)(foldOp).get.asInstanceOf[Value[IT, IV]]
    }
  }
}

object Fold {
  def sum(input: Value[IndexedSeq[Value[Long, SInt.type]], SCollection[Long, SInt.type]]) =
    Fold(input, 21, IntConstant(0), 22, Plus(TaggedInt(22), TaggedInt(21)))

  def sumBytes(input: Value[IndexedSeq[Value[Array[Byte], SByteArray.type]], SCollection[Array[Byte], SByteArray.type]]) =
    Fold[Array[Byte], SByteArray.type](input, 21, EmptyByteArray, 22, AppendBytes(TaggedByteArray(22), TaggedByteArray(21)))
}

case class ByIndex[T, V <: SType[T]](input: Value[IndexedSeq[Value[T, V]], SCollection[T, V]], index: Int)
  extends Transformer[IndexedSeq[Value[T, V]], SCollection[T, V], T, V] with NotReadyValue[T,V] {

  override def function(input: EvaluatedValue[IndexedSeq[Value[T, V]], SCollection[T,V]]) = input.value.apply(index)

  override def cost = 1
}


case class SizeOf[T, V <: SType[T]](input: Value[IndexedSeq[Value[T, V]], SCollection[T, V]])
  extends Transformer[IndexedSeq[Value[T, V]], SCollection[T, V], Long, SInt.type] with NotReadyValue[Long, SInt.type] {

  override def function(input: EvaluatedValue[IndexedSeq[Value[T, V]], SCollection[T, V]]) = IntConstant(input.value.length)

  override def cost = 1
}


sealed trait Extract[T, V <: SType[T]] extends Transformer[BoxWithMetadata, SBox.type, T, V] {
  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[T, V]
}

case class ExtractHeight(input: Value[BoxWithMetadata, SBox.type]) extends Extract[Long, SInt.type] with NotReadyValueInt {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[Long, SInt.type] =
    IntConstant(box.value.metadata.creationHeight)
}


case class ExtractAmount(input: Value[BoxWithMetadata, SBox.type]) extends Extract[Long, SInt.type] with NotReadyValueInt {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[Long, SInt.type] = IntConstant(box.value.box.value)
}


case class ExtractScript(input: Value[BoxWithMetadata, SBox.type]) extends Extract[Array[Byte], SProp.type] with NotReadyValueProp {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[Array[Byte], SProp.type] = {
    PropConstant(box.value)
  }
}


case class ExtractBytes(input: Value[BoxWithMetadata, SBox.type]) extends Extract[Array[Byte], SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[Array[Byte], SByteArray.type] = ByteArrayConstant(box.value.box.bytes)
}

case class ExtractId(input: Value[BoxWithMetadata, SBox.type]) extends Extract[Array[Byte], SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[Array[Byte], SByteArray.type] = ByteArrayConstant(box.value.box.id)
}

case class ExtractRegisterAs[T, V <: SType[T]](input: Value[BoxWithMetadata, SBox.type],
                                         registerId: RegisterIdentifier,
                                         default: Option[Value[T, V]] = None) extends Extract[T, V] with NotReadyValue[T, V] {
  override def cost: Int = 10

  override def function(box: EvaluatedValue[BoxWithMetadata, SBox.type]): Value[T, V] =
    box.value.box.get(registerId).map(_.asInstanceOf[Value[T, V]]).orElse(default).get
}