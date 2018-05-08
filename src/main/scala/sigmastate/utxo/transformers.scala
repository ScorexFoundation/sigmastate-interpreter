package sigmastate.utxo

import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.{Context, Interpreter}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.OpCodes
import sigmastate.utxo.BooleanTransformer.ResultConstructor
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.ErgoBox.RegisterIdentifier

import scala.collection.immutable


trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {

  val input: Value[IV]

  def transformationReady: Boolean = input.evaluated

  def function(input: EvaluatedValue[IV]): Value[OV]

  def function(): Value[OV] = input match {
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

  override val opCode: OpCode = OpCodes.MapCollectionCode

  val tpe = SCollection[OV]

  def arity = 4

  def deconstruct = immutable.Seq[Any](input, id, mapper, tOV)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SCollection[IV]]@unchecked,
    id: Byte,
    mapper: Transformer[IV, OV],
    t: OV@unchecked) => MapCollection[IV, OV](input, id, mapper)(t)
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

  /**
    * We consider transformation cost as size of collection * cost of trasfomation of one element of the collection.
    * We also need to add cost of resulting collection, but as we can not calculate it in prior, we assume that cost of
    * resulting collection is no more than cost of initial collection, so we taker latter as the estimation.
    *
    * @param context
    * @tparam C
    * @return
    */
  override def cost[C <: Context[C]](context: C): Int =
    Cost.MapDeclaration + input.cost(context) * (mapper.cost(context) + 1)
}

case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  val tpe = input.tpe

  override def transformationReady: Boolean =
    ConcreteCollection.isEvaluated(input) && ConcreteCollection.isEvaluated(col2)

  override def function(cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    ConcreteCollection(cl.value ++ col2.asInstanceOf[EvaluatedValue[SCollection[IV]]].value)(tpe.elemType)
  }

  override def cost[C <: Context[C]](context: C): Int = input.cost(context) + col2.cost(context)
}

case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  val tpe = input.tpe

  override def transformationReady: Boolean =
    ConcreteCollection.isEvaluated(input) && from.evaluated && until.evaluated

  override def function(cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val fromValue = from.asInstanceOf[EvaluatedValue[SInt.type]].value
    val untilValue = until.asInstanceOf[EvaluatedValue[SInt.type]].value
    ConcreteCollection(cl.value.slice(fromValue.toInt, untilValue.toInt))(tpe.elemType)
  }

  override def cost[C <: Context[C]](context: C): Int =
    input.cost(context) * 2 + from.cost(context) + until.cost(context)
}

case class Where[IV <: SType](input: Value[SCollection[IV]],
                              id: Byte,
                              condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def tpe: SCollection[IV] = input.tpe

  override def transformationReady: Boolean = ConcreteCollection.isEvaluated(input)

  override def cost[C <: Context[C]](context: C): Int =
    input.cost(context) * condition.cost(context) + input.cost(context)

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

trait BooleanTransformer[IV <: SType] extends Transformer[SCollection[IV], SBoolean.type] {
  override val input: Value[SCollection[IV]]
  val id: Byte
  val condition: Value[SBoolean.type]
  val f: ResultConstructor

  override def tpe = SBoolean

  override def transformationReady: Boolean = ConcreteCollection.isEvaluated(input)

  override def function(input: EvaluatedValue[SCollection[IV]]

                       ): Value[SBoolean.type] = {
    def rl(arg: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
    })

    f(input.value.map(el => rl(el)(condition).get.asInstanceOf[Value[SBoolean.type]]))
  }
}

object BooleanTransformer {
  type ResultConstructor = (Seq[Value[SBoolean.type]]) => Transformer[SCollection[SBoolean.type], SBoolean.type]
}

case class Exists[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends BooleanTransformer[IV] {
  override val opCode: OpCode = OpCodes.ExistsCode

  override def cost[C <: Context[C]](context: C): Int =
    Cost.ExistsDeclaration + input.cost(context) * condition.cost(context) + Cost.OrDeclaration

  override val f: ResultConstructor = OR.apply
}

case class ForAll[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends BooleanTransformer[IV] {

  override val opCode: OpCode = OpCodes.ForAllCode

  override def cost[C <: Context[C]](context: C) =
    Cost.ForAllDeclaration + input.cost(context) * condition.cost(context) + Cost.AndDeclaration

  override val f: ResultConstructor = AND.apply
}


case class Fold[IV <: SType](input: Value[SCollection[IV]],
                             id: Byte,
                             zero: Value[IV],
                             accId: Byte,
                             foldOp: SValue)(implicit val tpe: IV)
  extends Transformer[SCollection[IV], IV] with NotReadyValue[IV] with Rewritable {
  override val opCode: OpCode = OpCodes.FoldCode

  def arity = 6

  def deconstruct = immutable.Seq[Any](input, id, zero, accId, foldOp, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SCollection[IV]]@unchecked,
    id: Byte,
    zero: Value[IV],
    accId: Byte,
    foldOp: TwoArgumentsOperation[IV, IV, IV],
    t: IV@unchecked) => Fold[IV](input, id, zero, accId, foldOp)(t)
    case _ =>
      illegalArgs("Fold",
        "(Value[SCollection[IV]], id: Byte, zero: Value[IV], accId: Byte, foldOp: TwoArgumentsOperation[IV, IV, IV])(tpe: IV)", cs)
  }

  override def transformationReady: Boolean =
    input.evaluated &&
      input.asInstanceOf[ConcreteCollection[IV]].value.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[IV]]

  override def cost[C <: Context[C]](context: C): Int =
    Cost.FoldDeclaration + zero.cost(context) + input.cost(context) * foldOp.cost(context)

  override def function(input: EvaluatedValue[SCollection[IV]]): Value[IV] = {
    def rl(arg: Value[IV], acc: Value[IV]) = everywherebu(rule[Value[IV]] {
      case t: TaggedVariable[IV] if t.id == id => arg
      case t: TaggedVariable[IV] if t.id == accId => acc
    })

    input.value.foldLeft(zero) { case (acc: Value[IV], elem: Value[IV]) =>
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
  override val opCode: OpCode = OpCodes.ByIndexCode

  def tpe = input.tpe.elemType

  def arity = 3

  def deconstruct = immutable.Seq[Any](input, index, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SCollection[V]]@unchecked, index: Int, _) => ByIndex[V](input, index)
    case _ => illegalArgs("ByIndex", "(Value[SCollection[V]], index: Int)(tpe: V)", cs)
  }

  override def function(input: EvaluatedValue[SCollection[V]]) = input.value.apply(index)

  override def cost[C <: Context[C]](context: C) = input.cost(context) + Cost.ByIndexDeclaration
}

case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {

  override val opCode: OpCode = OpCodes.SizeOfCode

  override def function(input: EvaluatedValue[SCollection[V]]) = IntConstant(input.value.length)

  //todo: isn't this cost too high? we can get size of a collection without touching it
  override def cost[C <: Context[C]](context: C) = input.cost(context) + Cost.SizeOfDeclaration
}


sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
  override def function(box: EvaluatedValue[SBox.type]): Value[V]
}

case class ExtractAmount(input: Value[SBox.type]) extends Extract[SInt.type] with NotReadyValueInt {
  override val opCode: OpCode = OpCodes.ExtractAmountCode

  override def cost[C <: Context[C]](context: C) = 10

  override def function(box: EvaluatedValue[SBox.type]): Value[SInt.type] = IntConstant(box.value.value)
}


case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractScriptBytesCode

  override def cost[C <: Context[C]](context: C) = 1000

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = {
    ByteArrayConstant(box.value.propositionBytes)
  }
}


case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesCode

  override def cost[C <: Context[C]](context: C): Int = 1000 //todo: make it PerKb * max box size in kbs

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.bytes)
}

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode

  override def cost[C <: Context[C]](context: C) = 1000 //todo: make it PerKb * max box size in kbs

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.bytesWithNoRef)
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray.type] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractIdCode

  override def cost[C <: Context[C]](context: C) = 10

  override def function(box: EvaluatedValue[SBox.type]): Value[SByteArray.type] = ByteArrayConstant(box.value.id)
}

case class ExtractRegisterAs[V <: SType](input: Value[SBox.type],
                                         registerId: RegisterIdentifier,
                                         default: Option[Value[V]] = None)(implicit val tpe: V)
  extends Extract[V] with NotReadyValue[V] with Rewritable {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  def arity = 4

  def deconstruct = immutable.Seq[Any](input, registerId, default, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(input: Value[SBox.type]@unchecked,
    registerId: RegisterIdentifier,
    default: Option[Value[V]]@unchecked,
    t: V@unchecked) => ExtractRegisterAs[V](input, registerId, default)(t)
    case _ =>
      illegalArgs("ExtractRegisterAs", "(Value[SBox.type], registerId: RegisterIdentifier, default: Option[Value[V]])(tpe: V)", cs)
  }

  override def cost[C <: Context[C]](context: C) = 1000 //todo: the same as ExtractBytes.cost

  override def function(box: EvaluatedValue[SBox.type]): Value[V] =
    box.value.get(registerId).orElse(default).get.asInstanceOf[Value[V]]
}

trait Deserialize[V <: SType] extends NotReadyValue[V] with Rewritable


case class DeserializeContext[V <: SType](id: Byte)(implicit val tpe: V)
  extends Deserialize[V] {

  def arity = 2

  def deconstruct = immutable.Seq[Any](id, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(id: Byte@unchecked, t: V@unchecked) =>
      DeserializeContext[V](id)(t)
    case _ =>
      illegalArgs("DeserializeContext", "(Byte)(tpe: V)", cs)
  }

  override def cost[C <: Context[C]](context: C): Int = 1000 //todo: rework, consider limits
}


//todo: write test for this class
case class DeserializeRegister[V <: SType](reg: RegisterIdentifier,
                                           default: Option[Value[V]] = None)(implicit val tpe: V)
  extends Deserialize[V] {

  def arity = 3

  def deconstruct = immutable.Seq[Any](reg, default, tpe)

  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(reg: RegisterIdentifier@unchecked, default: Option[Value[V]]@unchecked, t: V@unchecked) =>
      DeserializeRegister[V](reg, default)(t)
    case _ =>
      illegalArgs("DeserializeRegister", "(RegisterIdentifier, Option[Value[V]])(tpe: V)", cs)
  }

  override def cost[C <: Context[C]](context: C): Int = 1000 //todo: rework, consider limits
}