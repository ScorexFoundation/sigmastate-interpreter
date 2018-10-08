package sigmastate.utxo

import org.ergoplatform._
import sigmastate.SCollection.{SBooleanArray, SByteArray}
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.interpreter.{Context, Interpreter}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.lang.exceptions.{InvalidType, OptionUnwrapNone}


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

  val opType = SCollection.MapMethod.stype.asFunc

  override def transformationReady: Boolean = input.isEvaluatedCollection

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
  val opType = SCollection.AppendMethod.stype.asFunc

  override def transformationReady: Boolean = input.isEvaluatedCollection && col2.isEvaluatedCollection

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
    input.isEvaluatedCollection && from.evaluated && until.evaluated

  override def function(intr: Interpreter, ctx: Context[_], cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val fromValue = from.asInstanceOf[EvaluatedValue[SInt.type]].value
    val untilValue = until.asInstanceOf[EvaluatedValue[SInt.type]].value
    val cc = cl.toConcreteCollection
    ConcreteCollection(cc.items.slice(fromValue, untilValue))(tpe.elemType)
  }

  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) * 2 + from.cost(context) + until.cost(context)

  def opType = {
    val tpeCol = SCollection(input.tpe.typeParams.head.asTypeIdent)
    SFunc(Vector(tpeCol, SInt, SInt), tpeCol)
  }
}

case class Where[IV <: SType](input: Value[SCollection[IV]],
                              id: Byte,
                              condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.WhereCode
  override def tpe: SCollection[IV] = input.tpe
  val opType = SCollection.WhereMethod.stype.asFunc

  override def transformationReady: Boolean = input.isEvaluatedCollection

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
      reduced match {
        case ev: EvaluatedValue[SBoolean.type] => ev.value
        case _ => Interpreter.error(s"Expected EvaluatedValue during execution of where but found $reduced")
      }
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

  override def transformationReady: Boolean = input.isEvaluatedCollection

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
  val opType = SCollection.ExistsMethod.stype.asFunc

  override def cost[C <: Context[C]](context: C): Long =
    Cost.ExistsDeclaration + input.cost(context) * condition.cost(context) + Cost.OrDeclaration

  override def constructResult(items: Seq[Value[SBoolean.type]]): Transformer[SBooleanArray, SBoolean.type] = OR(items)
}

case class ForAll[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends BooleanTransformer[IV] {

  override val opCode: OpCode = OpCodes.ForAllCode
  val opType = SCollection.ForallMethod.stype.asFunc

  override def cost[C <: Context[C]](context: C) =
    Cost.ForAllDeclaration + input.cost(context) * condition.cost(context) + Cost.AndDeclaration

  override def constructResult(items: Seq[Value[SBoolean.type]]): Transformer[SBooleanArray, SBoolean.type] = AND(items)
}


case class Fold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                             id: Byte,
                             zero: Value[OV],
                             accId: Byte,
                             foldOp: SValue)
  extends Transformer[SCollection[IV], OV] with NotReadyValue[OV] {
  override val opCode: OpCode = OpCodes.FoldCode
  implicit def tpe = zero.tpe
  val opType = SCollection.FoldMethod.stype.asFunc

  override def transformationReady: Boolean =
    input.evaluated &&
      input.items.forall(_.evaluated) &&
      zero.isInstanceOf[EvaluatedValue[OV]]

  override def cost[C <: Context[C]](context: C): Long =
    Cost.FoldDeclaration + zero.cost(context) + input.cost(context) * foldOp.cost(context)

  override def function(I: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[IV]]): Value[OV] = {
    val cc = input.toConcreteCollection
    cc.items.foldLeft(zero) { case (x, y) =>
      val (acc: EvaluatedValue[OV], elem: EvaluatedValue[IV]) = (x, y)
      val localCtx = ctx.withBindings(id -> elem, accId -> acc)
      val res = I.eval(localCtx, foldOp.asValue[OV])
      res
    }
  }
}

object Fold {
  def sum[T <: SNumericType](input: Value[SCollection[T]])(implicit tT: T) =
    Fold(input, 22, Constant(tT.upcast(0.toByte), tT), 21, Plus(TaggedVariable(21, tT), TaggedVariable(22, tT)))

  def concat[T <: SType](input: Value[SCollection[SCollection[T]]])(implicit tT: T): Fold[SCollection[T], T] = {
    val tCol = SCollection(tT)
    Fold[SCollection[T], T](
      input, 22, ConcreteCollection()(tT).asValue[T], 21,
      Append(TaggedVariable(21, tCol), TaggedVariable(22, tCol)))
  }
}

case class ByIndex[V <: SType](input: Value[SCollection[V]],
                               index: Value[SInt.type],
                               default: Option[Value[V]] = None)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] {
  override val opCode: OpCode = OpCodes.ByIndexCode
  override val tpe = input.tpe.elemType
  val opType = SCollection.ApplyMethod.stype.asFunc

  override def transformationReady: Boolean =
    input.isEvaluatedCollection && index.evaluated && default.forall(_.evaluated)

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[SCollection[V]]): Value[V] = {
    val i = index.asInstanceOf[EvaluatedValue[SInt.type]].value
    input.matchCase(
      cc =>
        cc.items.lift(i).orElse(default).get,
      const =>
        Value.apply(tpe)(const.value.lift(i).orElse(default).get.asInstanceOf[tpe.WrappedType]),
      tuple =>
        tuple.items.lift(i).orElse(default).get.asValue[V]
    )
  }
  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) + Cost.ByIndexDeclaration + default.map(_.cost(context)).getOrElse(0L)
}

/** Select tuple field by its 1-based index. E.g. input._1 is transformed to SelectField(input, 1)*/
case class SelectField(input: Value[STuple], fieldIndex: Byte)
  extends Transformer[STuple, SType] with NotReadyValue[SType] {
  override val opCode: OpCode = OpCodes.SelectFieldCode
  override val tpe = input.tpe.items(fieldIndex - 1)
  val opType = SFunc(input.tpe, tpe)

  override def transformationReady: Boolean = input.isEvaluatedCollection

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[STuple]): Value[SType] = {
    val item = input.value(fieldIndex - 1)
    Value.apply(tpe)(item.asInstanceOf[tpe.WrappedType])
  }
  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) + Cost.SelectFieldDeclaration
}

/** Represents execution of Sigma protocol that validates the given input SigmaProp. */
case class SigmaPropIsValid(input: Value[SSigmaProp.type])
    extends Transformer[SSigmaProp.type, SBoolean.type] with NotReadyValueBoolean {
  override val opCode: OpCode = OpCodes.SigmaPropIsValidCode
  override def transformationReady: Boolean = input.isInstanceOf[EvaluatedValue[_]]

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[SSigmaProp.type]): Value[SBoolean.type] = {
    input.value
  }
  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) + Cost.SigmaPropIsValidDeclaration

  def opType = SFunc(input.tpe, SBoolean)
}

/** Extract serialized bytes of a SigmaProp value */
case class SigmaPropBytes(input: Value[SSigmaProp.type])
    extends Transformer[SSigmaProp.type, SByteArray] with NotReadyValue[SByteArray] {
  override val opCode: OpCode = OpCodes.SigmaPropBytesCode
  def tpe = SByteArray
  val opType = SFunc(input.tpe, tpe)

  override def transformationReady: Boolean = input.isInstanceOf[EvaluatedValue[_]]

  override def function(intr: Interpreter, ctx: Context[_], input: EvaluatedValue[SSigmaProp.type]): Value[SByteArray] = {
    ByteArrayConstant(input.value.bytes)
  }
  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) + Cost.SigmaPropBytesDeclaration
}

/** Decode a SigmaProp(ProveDlog) from an ergo address */
case class ErgoAddressToSigmaProp(input: Value[SString.type])
  extends Transformer[SString.type, SSigmaProp.type] with NotReadyValue[SSigmaProp.type] {
  override val opCode: OpCode = OpCodes.ErgoAddressToSigmaPropCode

  override def function(intr: Interpreter, ctx: Context[_], bal: EvaluatedValue[SString.type]): Value[SSigmaProp.type] =
    intr match {
      case _: ErgoLikeInterpreter if ctx.isInstanceOf[ErgoLikeContext] =>
        ErgoAddressEncoder(ctx.asInstanceOf[ErgoLikeContext].metadata.networkPrefix)
          .fromString(bal.value)
          .get match {
          case a: P2PKAddress => a.pubkey
          case a@_ => Interpreter.error(s"unsupported address $a")
        }
      case i => Interpreter.error(s"unsupported interpreter $i")
    }

  override def cost[C <: Context[C]](context: C): Long =
    input.cost(context) + Cost.ParseSigmaProp

  override def tpe: SSigmaProp.type = SSigmaProp
}

case class SizeOf[V <: SType](input: Value[SCollection[V]])
    extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {
  override val opCode: OpCode = OpCodes.SizeOfCode
  val opType = SFunc(SCollection(SCollection.tIV), SInt)

  override def transformationReady: Boolean = input.isEvaluatedCollection
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
  override def cost[C <: Context[C]](context: C) = Cost.ExtractAmount
  val opType = SFunc(SBox, SLong)

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SLong.type] =
    LongConstant(box.value.value)
}


case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractScriptBytesCode
  override def cost[C <: Context[C]](context: C) = 1000
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.propositionBytes)
}


case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesCode
  override def cost[C <: Context[C]](context: C): Long = 1000 //todo: make it PerKb * max box size in kbs
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.bytes)
}

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode
  override def cost[C <: Context[C]](context: C) = 1000 //todo: make it PerKb * max box size in kbs
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.bytesWithNoRef)
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractIdCode
  override def cost[C <: Context[C]](context: C) = 10
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.id)
}

case class ExtractRegisterAs[V <: SType](
                                          input: Value[SBox.type],
                                          registerId: RegisterId,
                                          override val tpe: SOption[V])
  extends Extract[SOption[V]] with NotReadyValue[SOption[V]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs
  override def cost[C <: Context[C]](context: C) = 1000 //todo: the same as ExtractBytes.cost
  override def function(intr: Interpreter, ctx: Context[_], box: EvaluatedValue[SBox.type]): Value[SOption[V]] = {
    box.value.get(registerId) match {
      case Some(res) if res.tpe == tpe.elemType =>
        SomeValue(res.asInstanceOf[Value[V]])
      case Some(res) if res.tpe != tpe.elemType =>
        throw new InvalidType(s"Invalid value type ${res.tpe} in register $registerId, expected ${tpe.elemType}")
      case _ =>
        NoneValue(tpe.elemType)
    }
  }

  def opType = SFunc(Vector(SBox, SByte), tpe)
}

object ExtractRegisterAs {
  def apply[V <: SType](input: Value[SBox.type],
                        registerId: RegisterId)(implicit tpe: V): ExtractRegisterAs[V] =
    ExtractRegisterAs(input, registerId, SOption(tpe))
}

trait Deserialize[V <: SType] extends NotReadyValue[V]


case class DeserializeContext[V <: SType](id: Byte, tpe: V) extends Deserialize[V] {
  override val opCode: OpCode = OpCodes.DeserializeContextCode
  override def cost[C <: Context[C]](context: C): Long = 1000 //todo: rework, consider limits
  val opType = SFunc(Vector(SContext, SByte), tpe)
}


//todo: write test for this class
case class DeserializeRegister[V <: SType](reg: RegisterId, tpe: V, default: Option[Value[V]] = None) extends Deserialize[V] {
  override val opCode: OpCode = OpCodes.DeserializeRegisterCode
  override def cost[C <: Context[C]](context: C): Long = 1000 //todo: rework, consider limits
  val opType = SFunc(Vector(SBox, SByte, SOption(tpe)), tpe)
}

case class GetVar[V <: SType](varId: Byte, override val tpe: SOption[V]) extends NotReadyValue[SOption[V]] {
  override val opCode: OpCode = OpCodes.GetVarCode
  override def cost[C <: Context[C]](context: C): Long = context.extension.cost(varId) + 1
}

object GetVar {
  def apply[V <: SType](varId: Byte, innerTpe: V): GetVar[V] = GetVar[V](varId, SOption(innerTpe))

}

case class OptionGet[V <: SType](input: Value[SOption[V]]) extends Transformer[SOption[V], V] {
  override val opCode: OpCode = OpCodes.OptionGetCode
  override def tpe: V = input.tpe.elemType
  override def function(int: Interpreter, ctx: Context[_], input: EvaluatedValue[SOption[V]]): Value[V] =
    input match {
      case SomeValue(v) => v
      case n @ NoneValue(_) => throw new OptionUnwrapNone(s"Cannot unwrap None: $n")
    }
  override def cost[C <: Context[C]](context: C): Long = input.cost(context) + Cost.OptionGet
}

case class OptionGetOrElse[V <: SType](input: Value[SOption[V]], default: Value[V])
  extends Transformer[SOption[V], V] {
  override val opCode: OpCode = OpCodes.OptionGetOrElseCode
  override def tpe: V = input.tpe.elemType
  override def function(int: Interpreter, ctx: Context[_], input: EvaluatedValue[SOption[V]]): Value[V] =
    input match {
      case SomeValue(v) => v
      case NoneValue(_) => default
    }
  override def cost[C <: Context[C]](context: C): Long = input.cost(context) + Cost.OptionGetOrElse
}

case class OptionIsDefined[V <: SType](input: Value[SOption[V]])
  extends Transformer[SOption[V], SBoolean.type] {
  override val opCode: OpCode = OpCodes.OptionIsDefinedCode
  override def tpe= SBoolean
  override def function(int: Interpreter, ctx: Context[_], input: EvaluatedValue[SOption[V]]): Value[SBoolean.type] =
    input match {
      case SomeValue(_) => TrueLeaf
      case NoneValue(_) => FalseLeaf
    }
  override def cost[C <: Context[C]](context: C): Long = input.cost(context) + Cost.OptionIsDefined
}

case class MapCollection1[IV <: SType, OV <: SType](
    input: Value[SCollection[IV]],
    mapper: Value[SFunc])
    extends NotReadyValue[SCollection[OV]] {
  override val opCode: OpCode = OpCodes.MapCollectionCode
  val tpe = SCollection[OV](mapper.tpe.tRange.asInstanceOf[OV])
  def cost[C <: Context[C]](context: C) = ???
  val opType = SCollection.MapMethod.stype.asFunc
}

case class Exists1[IV <: SType](input: Value[SCollection[IV]], condition: Value[SFunc])
    extends NotReadyValue[SBoolean.type] {
  override val opCode: OpCode = OpCodes.ExistsCode
  override def tpe = SBoolean
  override def cost[C <: Context[C]](context: C): Long = ???
  val opType = SCollection.ExistsMethod.stype.asFunc
}

case class ForAll1[IV <: SType](input: Value[SCollection[IV]], condition: Value[SFunc])
    extends NotReadyValue[SBoolean.type] {
  override val opCode: OpCode = OpCodes.ForAllCode
  override def tpe = SBoolean
  override def cost[C <: Context[C]](context: C) = ???
  val opType = SCollection.ForallMethod.stype.asFunc
}

