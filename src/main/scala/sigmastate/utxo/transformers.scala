package sigmastate.utxo

import com.google.common.primitives.Shorts
import org.ergoplatform._
import sigmastate.SCollection.{SBooleanArray, SByteArray}
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.interpreter.{Context, Interpreter}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import org.ergoplatform.ErgoBox.{R3, RegisterId}
import sigmastate.lang.exceptions.OptionUnwrapNone
import special.sigma.InvalidType


trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {

  val input: Value[IV]

  def transformationReady: Boolean = input.evaluated

  def function(int: Interpreter, ctx: Context, input: EvaluatedValue[IV]): Value[OV]

  def function(int: Interpreter, ctx: Context): Value[OV] = input match {
    case ev: EvaluatedValue[IV] => function(int, ctx, ev)
    case _ => Interpreter.error(s"Transformer function can be called only after input value is evaluated: $input")
  }

  def evaluate(interp: Interpreter, ctx: Context): Value[OV] = input match {
    case ev: EvaluatedValue[IV] => function(interp, ctx, ev)
    case _: NotReadyValue[OV] => this
  }
}

case class MapCollection[IV <: SType, OV <: SType](
                                                    input: Value[SCollection[IV]],
                                                    mapper: Value[SFunc])
  extends Transformer[SCollection[IV], SCollection[OV]] {

  override val opCode: OpCode = OpCodes.MapCollectionCode

  implicit def tOV = mapper.asValue[OV].tpe

  override val tpe = SCollection[OV](mapper.tpe.tRange.asInstanceOf[OV])

  override val opType = SCollection.MapMethod.stype.asFunc

  override def transformationReady: Boolean = input.isEvaluatedCollection

  override def function(I: Interpreter, ctx: Context, cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[OV]] = ???
}

case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.AppendCode

  val tpe = input.tpe
  val opType = SCollection.AppendMethod.stype.asFunc

  override def transformationReady: Boolean = input.isEvaluatedCollection && col2.isEvaluatedCollection

  override def function(intr: Interpreter, ctx: Context, cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val c1 = cl.toConcreteCollection
    val c2 = col2.toConcreteCollection
    ConcreteCollection(c1.items ++ c2.items)(tpe.elemType)
  }
}

case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.SliceCode

  val tpe = input.tpe

  override def transformationReady: Boolean =
    input.isEvaluatedCollection && from.evaluated && until.evaluated

  override def function(intr: Interpreter, ctx: Context, cl: EvaluatedValue[SCollection[IV]]): Value[SCollection[IV]] = {
    val fromValue = from.asInstanceOf[EvaluatedValue[SInt.type]].value
    val untilValue = until.asInstanceOf[EvaluatedValue[SInt.type]].value
    val cc = cl.toConcreteCollection
    ConcreteCollection(cc.items.slice(fromValue, untilValue))(tpe.elemType)
  }

  def opType = {
    val tpeCol = SCollection(input.tpe.typeParams.head.ident)
    SFunc(Vector(tpeCol, SInt, SInt), tpeCol)
  }
}

case class Filter[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override val opCode: OpCode = OpCodes.FilterCode
  override def tpe: SCollection[IV] = input.tpe
  val opType = SCollection.FilterMethod.stype.asFunc

  override def transformationReady: Boolean = input.isEvaluatedCollection

  override def function(intr: Interpreter, ctx: Context, input: EvaluatedValue[SCollection[IV]]): ConcreteCollection[IV] = {
    val cc = input.toConcreteCollection
    val filtered = cc.items.filter { case v: EvaluatedValue[IV] =>
      val localCtx = ctx.withBindings(id -> v)
      val reduced = intr.eval(localCtx, condition)
      reduced match {
        case ev: EvaluatedValue[SBoolean.type] => ev.value
        case _ => Interpreter.error(s"Expected EvaluatedValue during execution of filter but found $reduced")
      }
    }
    ConcreteCollection(filtered)(tpe.elemType)
  }
}

trait BooleanTransformer[IV <: SType] extends Transformer[SCollection[IV], SBoolean.type] {
  override val input: Value[SCollection[IV]]
  val condition: Value[SFunc]

  override def tpe = SBoolean

  override def transformationReady: Boolean = input.isEvaluatedCollection

  override def function(I: Interpreter, ctx: Context, input: EvaluatedValue[SCollection[IV]]): Value[SBoolean.type] = ???
}

case class Exists[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override val opCode: OpCode = OpCodes.ExistsCode
  val opType = SCollection.ExistsMethod.stype.asFunc
}

case class ForAll[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {

  override val opCode: OpCode = OpCodes.ForAllCode
  val opType = SCollection.ForallMethod.stype.asFunc
}


case class Fold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                          zero: Value[OV],
                                          foldOp: Value[SFunc])
  extends Transformer[SCollection[IV], OV] {
  override val opCode: OpCode = OpCodes.FoldCode

  implicit def tpe: OV = zero.tpe
  val opType: SFunc = SCollection.FoldMethod.stype.asFunc

  override def transformationReady: Boolean =
    input.isEvaluatedCollection &&
      zero.isInstanceOf[EvaluatedValue[OV]]

  override def function(I: Interpreter, ctx: Context, input: EvaluatedValue[SCollection[IV]]): Value[OV] = ???
}

object Fold {
  def sum[T <: SNumericType](input: Value[SCollection[T]])(implicit tT: T) =
    Fold(input,
      Constant(tT.upcast(0.toByte), tT),
      FuncValue(Vector((1, STuple(tT, tT))),
        Plus(
          SelectField(ValUse(1, STuple(tT, tT)), 1).asNumValue,
          SelectField(ValUse(1, STuple(tT, tT)), 2).asNumValue))
    )

  def concat[T <: SType](input: Value[SCollection[SCollection[T]]])(implicit tT: T): Fold[SCollection[T], T] = {
    val tCol = SCollection(tT)
    Fold[SCollection[T], T](input,
      ConcreteCollection()(tT).asValue[T],
      FuncValue(Vector((1, tCol), (2, tCol)), Append(ValUse(1, tCol), ValUse(2, tCol)))
    )
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

  override def function(intr: Interpreter, ctx: Context, input: EvaluatedValue[SCollection[V]]): Value[V] = {
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
}

/** Select tuple field by its 1-based index. E.g. input._1 is transformed to SelectField(input, 1) */
case class SelectField(input: Value[STuple], fieldIndex: Byte)
  extends Transformer[STuple, SType] with NotReadyValue[SType] {
  override val opCode: OpCode = OpCodes.SelectFieldCode
  override val tpe = input.tpe.items(fieldIndex - 1)
  val opType = SFunc(input.tpe, tpe)

  override def transformationReady: Boolean = input.isEvaluatedCollection

  override def function(intr: Interpreter, ctx: Context, input: EvaluatedValue[STuple]): Value[SType] = {
    val item = input.value(fieldIndex - 1)
    Value.apply(tpe)(item.asInstanceOf[tpe.WrappedType])
  }
}

/** Represents execution of Sigma protocol that validates the given input SigmaProp. */
case class SigmaPropIsValid(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SBoolean.type] with NotReadyValueBoolean {
  override val opCode: OpCode = OpCodes.SigmaPropIsValidCode

  override def transformationReady: Boolean = input.isInstanceOf[EvaluatedValue[_]]

  override def function(intr: Interpreter, ctx: Context, input: EvaluatedValue[SSigmaProp.type]): Value[SBoolean.type] = {
    input.value
  }

  def opType = SFunc(input.tpe, SBoolean)
}

/** Extract serialized bytes of a SigmaProp value */
case class SigmaPropBytes(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SByteArray] with NotReadyValue[SByteArray] {
  override val opCode: OpCode = OpCodes.SigmaPropBytesCode

  def tpe = SByteArray
  val opType = SFunc(input.tpe, tpe)

  override def transformationReady: Boolean = input.isInstanceOf[EvaluatedValue[_]]

  override def function(intr: Interpreter, ctx: Context, input: EvaluatedValue[SSigmaProp.type]): Value[SByteArray] = {
    ByteArrayConstant(input.value.bytes)
  }
}

case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {
  override val opCode: OpCode = OpCodes.SizeOfCode
  val opType = SFunc(SCollection(SCollection.tIV), SInt)

  override def transformationReady: Boolean = input.isEvaluatedCollection

  override def function(intr: Interpreter, ctx: Context, input: EvaluatedValue[SCollection[V]]) =
    IntConstant(input.length)
}


sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[V]
}

case class ExtractAmount(input: Value[SBox.type]) extends Extract[SLong.type] with NotReadyValueLong {
  override val opCode: OpCode = OpCodes.ExtractAmountCode
  val opType = SFunc(SBox, SLong)

  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[SLong.type] =
    LongConstant(box.value.value)
}


case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractScriptBytesCode
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.propositionBytes)
}


case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesCode
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.bytes)
}

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.bytesWithNoRef)
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.ExtractIdCode
  val opType = SFunc(SBox, SByteArray)

  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[SByteArray] =
    ByteArrayConstant(box.value.id)
}

case class ExtractRegisterAs[V <: SType](
                                          input: Value[SBox.type],
                                          registerId: RegisterId,
                                          override val tpe: SOption[V])
  extends Extract[SOption[V]] with NotReadyValue[SOption[V]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[SOption[V]] = {
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

case class ExtractCreationInfo(input: Value[SBox.type]) extends Extract[STuple] with NotReadyValue[STuple] {
  import ExtractCreationInfo._
  @inline def tpe: STuple = ResultType
  override val opCode: OpCode = OpCodes.ExtractCreationInfoCode
  @inline def opType = OpType
  override def function(intr: Interpreter, ctx: Context, box: EvaluatedValue[SBox.type]): Value[STuple] =
      box.value.get(ErgoBox.ReferenceRegId).get.asValue[STuple]
}
object ExtractCreationInfo {
  val ResultType = STuple(SLong, SByteArray)
  val OpType = SFunc(SBox, ResultType)
}

trait Deserialize[V <: SType] extends NotReadyValue[V]


case class DeserializeContext[V <: SType](id: Byte, tpe: V) extends Deserialize[V] {
  override val opCode: OpCode = OpCodes.DeserializeContextCode
  val opType = SFunc(Vector(SContext, SByte), tpe)
}


//todo: write test for this class
case class DeserializeRegister[V <: SType](reg: RegisterId, tpe: V, default: Option[Value[V]] = None) extends Deserialize[V] {
  override val opCode: OpCode = OpCodes.DeserializeRegisterCode
  override val opType = SFunc(Vector(SBox, SByte, SOption(tpe)), tpe)
}

case class GetVar[V <: SType](varId: Byte, override val tpe: SOption[V]) extends NotReadyValue[SOption[V]] {
  override val opCode: OpCode = OpCodes.GetVarCode
  override val opType = SFunc(Vector(SContext, SByte), tpe)
}

object GetVar {
  def apply[V <: SType](varId: Byte, innerTpe: V): GetVar[V] = GetVar[V](varId, SOption(innerTpe))
}

case class OptionGet[V <: SType](input: Value[SOption[V]]) extends Transformer[SOption[V], V] {
  override val opCode: OpCode = OpCodes.OptionGetCode
  override val opType = SFunc(input.tpe, tpe)
  override def tpe: V = input.tpe.elemType
  override def function(int: Interpreter, ctx: Context, input: EvaluatedValue[SOption[V]]): Value[V] = ???
  override def toString: String = s"$input.get"
}

case class OptionGetOrElse[V <: SType](input: Value[SOption[V]], default: Value[V])
  extends Transformer[SOption[V], V] {
  override val opCode: OpCode = OpCodes.OptionGetOrElseCode
  override val opType = SFunc(IndexedSeq(input.tpe, tpe), tpe)
  override def tpe: V = input.tpe.elemType
  override def function(int: Interpreter, ctx: Context, input: EvaluatedValue[SOption[V]]): Value[V] = ???
}

case class OptionIsDefined[V <: SType](input: Value[SOption[V]])
  extends Transformer[SOption[V], SBoolean.type] {
  override val opCode: OpCode = OpCodes.OptionIsDefinedCode
  override val opType = SFunc(input.tpe, SBoolean)
  override def tpe= SBoolean
  override def function(int: Interpreter, ctx: Context, input: EvaluatedValue[SOption[V]]): Value[SBoolean.type] = ???
}
