package sigmastate.utxo

import sigmastate.SCollection.{SByteArray, SBooleanArray}
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.interpreter.{Context, Interpreter}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.OpCodes
import sigmastate.utils.Helpers
import sigmastate.utxo.CostTable.Cost
import org.ergoplatform.ErgoBox.RegisterIdentifier


trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {

  val input: Value[IV]

  def transformationReady: Boolean = input.evaluated

  def function(int: Interpreter, ctx: Context[_], input: EvaluatedValue[IV]): Value[OV]

  def function(int: Interpreter, ctx: Context[_]): Value[OV] = input match {
    case ev: EvaluatedValue[IV] => function(int, ctx, ev)
    case _ => Interpreter.error(s"Transformer function can be called only after input value is evaluated: $input")
  }

  def evaluate(interp: Interpreter, ctx: Context[_]): Value[OV] = input match {
    case ev: EvaluatedValue[IV] => function(interp, ctx, ev)
    case _: NotReadyValue[OV] => this
  }
}

case class MapCollection[IV <: SType, OV <: SType](
    input: Value[SCollection[IV]],
    id: Byte,
    mapper: SValue)
  extends Transformer[SCollection[IV], SCollection[OV]] {

  override val opCode: OpCode = OpCodes.MapCollectionCode
  implicit def tOV = mapper.asValue[OV].tpe
  val tpe = SCollection[OV](tOV)

  override def transformationReady: Boolean = input.isEvaluated

  override def function(I: Interpreter, ctx: Context[_], cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[OV]] = {
    val cc = cl.toConcreteCollection
    val resItems = cc.items.map {
      case v: EvaluatedValue[IV] =>
        val localCtx = ctx.withBindings(id -> v)
        val reduced = I.eval(localCtx, mapper.asValue[OV])
        reduced
      case v =>
        Interpreter.error(s"Error evaluating $this: value $v is not EvaluatedValue")
    }
    ConcreteCollection(resItems)
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
  override def cost[C <: Context[C]](context: C): Long =
    Cost.MapDeclaration + input.cost(context) * (mapper.cost(context) + 1)
}

case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.AppendCode

  val tpe = input.tpe

  override def transformationReady: Boolean = input.isEvaluated && col2.isEvaluated

  override def function(intr: Interpreter, ctx: Context[_], cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val c1 = cl.toConcreteCollection
    val c2 = col2.toConcreteCollection
    ConcreteCollection(c1.items ++ c2.items)(tpe.elemType)
  }

  override def cost[C <: Context[C]](context: C): Long = input.cost(context) + col2.cost(context)
}

case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.SliceCode

  val tpe = input.tpe

  override def transformationReady: Boolean =
    input.isEvaluated && from.evaluated && until.evaluated

  override def function(intr: Interpreter, ctx: Context[_], cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val fromValue = from.asInstanceOf[EvaluatedValue[SInt.type]].value
    val untilValue = until.asInstanceOf[EvaluatedValue[SInt.type]].value
    val cc = cl.toConcreteCollection
    ConcreteCollection(cc.items.slice(fromValue, untilValue))(tpe.elemType)
  }

  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) * 2 + from.cost(context) + until.cost(context)
}

case class Where[IV <: SType](input: Value[SCollection[IV]],
                              id: Byte,
                              condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.WhereCode

  override def tpe: SCollection[IV] = input.tpe

  override def transformationReady: Boolean = input.isEvaluated

  override def cost[C <: Context[C]](context: C): Long = {
    val elemType = input.tpe.elemType
    val data = Constant(null.asInstanceOf[elemType.WrappedType], elemType)
    val localCtx = context.withBindings(id -> data)
    Cost.WhereDeclaration + input.cost(context) * condition.cost(context) + input.cost(context)
  }

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[IV]]): ConcreteCollection[IV] = {
    val cc = input.toConcreteCollection
    val filtered = cc.items.filter { case v: EvaluatedValue[IV] =>
      val localCtx = ctx.withBindings(id -> v)
      val reduced = intr.eval(localCtx, condition)
      reduced.value
    }
    ConcreteCollection(filtered)(tpe.elemType)
  }
}

trait BooleanTransformer[IV <: SType] extends Transformer[SCollection[IV], SBoolean.type] {
  override val input: Value[SCollection[IV]]
  val id: Byte
  val condition: Value[SBoolean.type]
  def constructResult(items: Seq[Value[SBoolean.type]]): Transformer[SBooleanArray, SBoolean.type]

  override def tpe = SBoolean

  override def transformationReady: Boolean = input.isEvaluated

  override def function(I: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[IV]]): Value[SBoolean.type] = {
    val cc = input.toConcreteCollection
    val resItems = cc.items.map { case v: EvaluatedValue[IV] =>
      val localCtx = ctx.withBindings(id -> v)
      val reduced = I.eval(localCtx, condition)
      reduced
    }
    constructResult(resItems)
  }
}

case class Exists[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends BooleanTransformer[IV] {
  override val opCode: OpCode = OpCodes.ExistsCode

  override def cost[C <: Context[C]](context: C): Long =
    Cost.ExistsDeclaration + input.cost(context) * condition.cost(context) + Cost.OrDeclaration

  override def constructResult(items: Seq[Value[SBoolean.type]]): Transformer[SBooleanArray, SBoolean.type] = OR(items)
}

case class ForAll[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends BooleanTransformer[IV] {

  override val opCode: OpCode = OpCodes.ForAllCode

  override def cost[C <: Context[C]](context: C) =
    Cost.ForAllDeclaration + input.cost(context) * condition.cost(context) + Cost.AndDeclaration

  override def constructResult(items: Seq[Value[SBoolean.type]]): Transformer[SBooleanArray, SBoolean.type] = AND(items)
}


case class Fold[IV <: SType](input: Value[SCollection[IV]],
                             id: Byte,
                             zero: Value[IV],
                             accId: Byte,
                             foldOp: SValue)
  extends Transformer[SCollection[IV], IV] with NotReadyValue[IV] {
  override val opCode: OpCode = OpCodes.FoldCode
  implicit def tpe = input.tpe.elemType

  override def transformationReady: Boolean =
    input.evaluated &&
      input.items.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[IV]]

  override def cost[C <: Context[C]](context: C): Long =
    Cost.FoldDeclaration + zero.cost(context) + input.cost(context) * foldOp.cost(context)

  override def function(I: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[IV]]): Value[IV] = {
    val cc = input.toConcreteCollection
    cc.items.foldLeft(zero) { case (x, y) =>
      val (acc: EvaluatedValue[IV], elem: EvaluatedValue[IV]) = (x, y)
      val localCtx = ctx.withBindings(id -> elem, accId -> acc)
      val res = I.eval(localCtx, foldOp.asValue[IV])
      res
    }
  }
}

object Fold {
  def sum[T <: SNumericType](input: Value[SCollection[T]])(implicit tT: T) =
    Fold(input, 21, Constant(tT.upcast(0.toByte), tT), 22, Plus(TaggedVariable(22, tT), TaggedVariable(21, tT)))

  def concat[T <: SType](input: Value[SCollection[SCollection[T]]])(implicit tT: T) = {
    val tCol = SCollection(tT)
    Fold[SCollection[T]](
      input, 21, ConcreteCollection()(tT), 22,
      Append(TaggedVariable(22, tCol), TaggedVariable(21, tCol)))
  }
}

case class ByIndex[V <: SType](input: Value[SCollection[V]],
                               index: Value[SInt.type],
                               default: Option[Value[V]] = None)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] {
  override val opCode: OpCode = OpCodes.ByIndexCode
  override val tpe = input.tpe.elemType
  override def transformationReady: Boolean =
    input.isEvaluated && index.evaluated && default.forall(_.evaluated)

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[V]]): Value[V] = {
    val i = index.asInstanceOf[EvaluatedValue[SInt.type]].value
    input.matchCase(
      cc =>
        cc.items.lift(i).orElse(default).get,
      const =>
        Value.apply(tpe)(const.value.lift(i).orElse(default).get.asInstanceOf[tpe.WrappedType])
    )
  }
  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) + Cost.ByIndexDeclaration + default.map(_.cost(context)).getOrElse(0L)
}

case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {

  override val opCode: OpCode = OpCodes.SizeOfCode

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[V]]) =
    IntConstant(input.length)

  //todo: isn't this cost too high? we can get size of a collection without touching it
  override def cost[C <: Context[C]](context: C) = input.cost(context) + Cost.SizeOfDeclaration
}


sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[V]
}

case class ExtractAmount(input: Value[SBox.type]) extends Extract[SLong.type] with NotReadyValueLong {
  override val opCode: OpCode = OpCodes.ExtractAmountCode

  override def cost[C <: Context[C]](context: C) = 10

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SLong.type] =
    LongConstant(box.value.value)
}


case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractScriptBytesCode

  override def cost[C <: Context[C]](context: C) = 1000

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.propositionBytes)
}


case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesCode

  override def cost[C <: Context[C]](context: C): Long = 1000 //todo: make it PerKb * max box size in kbs

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.bytes)
}

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode

  override def cost[C <: Context[C]](context: C) = 1000 //todo: make it PerKb * max box size in kbs

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.bytesWithNoRef)
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractIdCode

  override def cost[C <: Context[C]](context: C) = 10

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.id)
}

case class ExtractRegisterAs[V <: SType](
    input: Value[SBox.type],
    registerId: RegisterIdentifier,
    tpe: V,
    default: Option[Value[V]])
    extends Extract[V] with NotReadyValue[V] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs
  override def cost[C <: Context[C]](context: C) = 1000 //todo: the same as ExtractBytes.cost
  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[V] = {
    val res = box.value.get(registerId).orElse(default).get
    if (res.tpe != this.tpe)
      Interpreter.error(s"Invalid value type ${res.tpe} in register R${registerId.number}, expected $tpe")
    res.asInstanceOf[Value[V]]
  }
}

object ExtractRegisterAs {
  def apply[V <: SType](input: Value[SBox.type],
      registerId: RegisterIdentifier,
      default: Option[Value[V]] = None)(implicit tpe: V): ExtractRegisterAs[V] =
    ExtractRegisterAs(input, registerId, tpe, default)
}

trait Deserialize[V <: SType] extends NotReadyValue[V]


case class DeserializeContext[V <: SType](id: Byte, tpe: V) extends Deserialize[V] {
  override val opCode: OpCode = OpCodes.DeserializeContextCode
  override def cost[C <: Context[C]](context: C): Long = 1000 //todo: rework, consider limits
}


//todo: write test for this class
case class DeserializeRegister[V <: SType](
    reg: RegisterIdentifier, tpe: V, default: Option[Value[V]] = None) extends Deserialize[V] {

  override val opCode: OpCode = OpCodes.DeserializeRegisterCode
  override def cost[C <: Context[C]](context: C): Long = 1000 //todo: rework, consider limits
}
