package sigmastate.utxo

import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.ErgoBox.RegisterIdentifier

import scala.collection.immutable


trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {

  val input: Value[IV]

  def transformationReady: Boolean = input.evaluated

  def function(input: EvaluatedValue[IV]): Value[OV]

  def function(): Value[OV] = input match{
    case ev: EvaluatedValue[IV] => function(ev)
    case _ => Interpreter.error(s"Transformer function can be called only after input value is evaluated: $input")
  }

  def evaluate(): Value[OV] = input match {
    case ev: EvaluatedValue[IV] => function(ev)
    case _: NotReadyValue[OV] => this
  }
}


case class MapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                                   id: Byte,
                                                   mapper: SValue)(implicit val tOV: OV)
  extends Transformer[SCollection[IV], SCollection[OV]] with Rewritable {
  val tpe = SCollection[OV]
  def arity = 4
  def deconstruct = immutable.Seq[Any](input, id, mapper, tOV)
  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SCollection[IV]] @unchecked,
             id: Byte,
             mapper: Transformer[IV, OV],
             t: OV @unchecked) => MapCollection[IV, OV](input, id, mapper)(t)
    case _ =>
      illegalArgs("MapCollection", "(Value[SCollection[IV]], Byte, Transformer[IV, OV], SType)", cs)
  }

  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated)

  override def function(cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[OV]] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    ConcreteCollection(cl.value.map(el => rl(el)(mapper).get.asInstanceOf[Transformer[IV, OV]]).map(_.function()))
  }

  override def cost: Int = input.cost * mapper.cost
}

case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]]{
  val tpe = input.tpe

  override def transformationReady: Boolean =
    ConcreteCollection.isEvaluated(input) && ConcreteCollection.isEvaluated(col2)

  override def function(cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    ConcreteCollection(cl.value ++ col2.asInstanceOf[EvaluatedValue[SCollection[IV]]].value)(tpe.elemType)
  }

  override def cost: Int = input.cost + col2.cost
}

case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]]{
  val tpe = input.tpe

  override def transformationReady: Boolean =
    ConcreteCollection.isEvaluated(input) && from.evaluated && until.evaluated

  override def function(cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val fromValue = from.asInstanceOf[EvaluatedValue[SInt.type]].value
    val untilValue = until.asInstanceOf[EvaluatedValue[SInt.type]].value
    ConcreteCollection(cl.value.slice(fromValue.toInt, untilValue.toInt))(tpe.elemType)
  }

  override def cost: Int = input.cost * 2 + from.cost + until.cost
}

case class Where[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def tpe: SCollection[IV] = input.tpe
  override def transformationReady: Boolean = ConcreteCollection.isEvaluated(input)

  override def cost: Int = input.cost * condition.cost + input.cost * Cost.OrDeclaration

  override def function(input: EvaluatedValue[SCollection[IV]]): ConcreteCollection[IV] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })
    def p(x: Value[IV]): Boolean = {
      val res = rl(x)(condition).get
      res.asInstanceOf[EvaluatedValue[SBoolean.type]].value
    }
    val filtered = input.value.filter(p)
    ConcreteCollection(filtered)(tpe.elemType)
  }
}

case class Exists[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SBoolean.type] {
  override def tpe = SBoolean
  override def transformationReady: Boolean = ConcreteCollection.isEvaluated(input)

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
  override def tpe = SBoolean
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


case class Fold[IV <: SType](input: Value[SCollection[IV]],
                             id: Byte,
                             zero: Value[IV],
                             accId: Byte,
                             foldOp: SValue)(implicit val tpe: IV)
  extends Transformer[SCollection[IV], IV] with NotReadyValue[IV] with Rewritable {
  def arity = 6
  def deconstruct = immutable.Seq[Any](input, id, zero, accId, foldOp, tpe)
  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SCollection[IV]] @unchecked,
             id: Byte,
             zero: Value[IV],
             accId: Byte,
             foldOp: TwoArgumentsOperation[IV, IV, IV],
             t: IV @unchecked) => Fold[IV](input, id, zero, accId, foldOp)(t)
    case _ =>
      illegalArgs("Fold",
        "(Value[SCollection[IV]], id: Byte, zero: Value[IV], accId: Byte, foldOp: TwoArgumentsOperation[IV, IV, IV])(tpe: IV)", cs)
  }

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
    Fold[SByteArray.type](input, 21, ByteArrayConstant(Array.emptyByteArray), 22, AppendBytes(TaggedByteArray(22), TaggedByteArray(21)))
}

case class ByIndex[V <: SType](input: Value[SCollection[V]], index: Int)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] with Rewritable {
  def tpe = input.tpe.elemType
  def arity = 3
  def deconstruct = immutable.Seq[Any](input, index, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SCollection[V]]@unchecked, index: Int, _) => ByIndex[V](input, index)
    case _ => illegalArgs("ByIndex", "(Value[SCollection[V]], index: Int)(tpe: V)", cs)
  }
  override def function(input: EvaluatedValue[SCollection[V]]) = input.value.apply(index)

  override def cost = 1
}

case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {

  override def function(input: EvaluatedValue[SCollection[V]]) = IntConstant(input.value.length)

  override def cost = 1
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

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 1000 //todo: make it PerKb * max box size in kbs

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.bytesWithNoRef)
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override lazy val cost: Int = 10

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.id)
}

case class ExtractRegisterAs[V <: SType](input: Value[SBox.type],
                                         registerId: RegisterIdentifier,
                                         default: Option[Value[V]] = None)(implicit val tpe: V)
    extends Extract[V] with NotReadyValue[V] with Rewritable {
  def arity = 4
  def deconstruct = immutable.Seq[Any](input, registerId, default, tpe)
  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SBox.type] @unchecked,
      registerId: RegisterIdentifier,
      default: Option[Value[V]] @unchecked,
      t: V @unchecked) => ExtractRegisterAs[V](input, registerId, default)(t)
    case _ =>
      illegalArgs("ExtractRegisterAs", "(Value[SBox.type], registerId: RegisterIdentifier, default: Option[Value[V]])(tpe: V)", cs)
  }
  override def cost: Int = 1000 //todo: the same as ExtractBytes.cost

  override def function(box: EvaluatedValue[SBox.type]): Value[V] =
    box.value.get(registerId).orElse(default).get.asInstanceOf[Value[V]]
}



case class Deserialize[V <: SType](input: Value[SByteArray.type])(implicit val tpe: V)
  extends Transformer[SByteArray.type, V] with NotReadyValue[V] with Rewritable {

  def arity = 2

  def deconstruct = immutable.Seq[Any](input, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SByteArray.type]@unchecked, t: V@unchecked) =>
      Deserialize[V](input)(t)
    case _ =>
      illegalArgs("Deserialize", "(Value[SByteArray.type])(tpe: V)", cs)
  }

  override def cost: Int = 1000 //todo: consider limits

  override def function(box: EvaluatedValue[SByteArray.type]): Value[V] =
    ValueSerializer.deserialize(box.value).asInstanceOf[Value[V]]
}