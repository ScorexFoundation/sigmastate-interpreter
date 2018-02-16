package sigmastate.utxo

import sigmastate.{NotReadyValueInt, _}
import sigmastate.utxo.SigmaStateBox.RegisterIdentifier
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}
import sigmastate.utxo.CostTable.Cost


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


case class MapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                                   id: Byte,
                                                   mapper: Transformer[IV, OV])
  extends Transformer[SCollection[IV], SCollection[OV]] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override def function(cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[OV]] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    ConcreteCollection(cl.value.map(el => (rl(el)(mapper)).get.asInstanceOf[Transformer[IV, OV]]).map(_.function()))
  }

  override def cost: Int = input.cost * mapper.cost
}

case class Exists[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SBoolean.type] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost * condition.cost + input.cost * Cost.OrDeclaration

  //todo: cost
  override def function(input: EvaluatedValue[SCollection[IV]]): Value[SBoolean.type] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    OR(input.value.map(el => rl(el)(condition).get.asInstanceOf[Value[SBoolean.type]]))
  }
}

case class ForAll[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SBoolean.type] {

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override val cost: Int = input.cost * condition.cost + input.cost * Cost.AndDeclaration

  //todo: cost
  override def function(input: EvaluatedValue[SCollection[IV]]): Value[SBoolean.type] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    AND(input.value.map(el => rl(el)(condition).get.asInstanceOf[Value[SBoolean.type]]))
  }
}

/*
todo: implement

object Append
object Slice
*/


case class Fold[IV <: SType](input: Value[SCollection[IV]],
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
  def sum(input: Value[SCollection[SInt.type]]) =
    Fold(input, 21, IntConstant(0), 22, Plus(TaggedInt(22), TaggedInt(21)))

  def sumBytes(input: Value[SCollection[SByteArray.type]]) =
    Fold[SByteArray.type](input, 21, EmptyByteArray, 22, AppendBytes(TaggedByteArray(22), TaggedByteArray(21)))
}

case class ByIndex[V <: SType](input: Value[SCollection[V]], index: Int)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] {

  override def function(input: EvaluatedValue[SCollection[V]]) = input.value.apply(index)

  override def cost = 1
}


case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValue[SInt.type] {

  override def function(input: EvaluatedValue[SCollection[V]]) = IntConstant(input.value.length)

  override def cost = 1
}



case class ExtractBox(input: Value[SBoxWithMetadata.type])
  extends Transformer[SBoxWithMetadata.type, SBox.type] with NotReadyValueBox {

  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[SBoxWithMetadata.type]): Value[SBox.type] =
    BoxConstant(box.value.box)
}

case class ExtractHeight(input: Value[SBoxWithMetadata.type])
  extends Transformer[SBoxWithMetadata.type, SInt.type] with NotReadyValueInt {

  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[SBoxWithMetadata.type]): Value[SInt.type] =
    IntConstant(box.value.metadata.creationHeight)
}



sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
  override def function(box: EvaluatedValue[SBox.type]): Value[V]
}

case class ExtractAmount(input: Value[SBox.type]) extends Extract[SInt.type] with NotReadyValueInt {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[SBox.type]): Value[SInt.type] = IntConstant(box.value.value)
}


case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 1000

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = {
    ByteArrayConstant(box.value.propositionBytes)
  }
}


case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 1000 //todo: make it PerKb * max box size in kbs

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.bytes)
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.id)
}

case class ExtractRegisterAs[V <: SType](input: Value[SBox.type],
                                         registerId: RegisterIdentifier,
                                         default: Option[Value[V]] = None) extends Extract[V] with NotReadyValue[V] {
  override def cost: Int = 1000 //todo: the same as ExtractBytes.cost

  override def function(box: EvaluatedValue[SBox.type]): Value[V] =
    box.value.get(registerId).orElse(default).get.asInstanceOf[Value[V]]
}