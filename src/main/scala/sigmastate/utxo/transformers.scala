package sigmastate.utxo

import com.google.common.primitives.Shorts
import org.ergoplatform._
import sigmastate.SCollection.{SBooleanArray, SByteArray}
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.interpreter.{InterpreterContext, Interpreter}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import org.ergoplatform.ErgoBox.{R3, RegisterId}
import sigmastate.lang.exceptions.{OptionUnwrapNone, InterpreterException}
import special.sigma.InvalidType


trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {
  val input: Value[IV]
}

case class MapCollection[IV <: SType, OV <: SType](
                                                    input: Value[SCollection[IV]],
                                                    mapper: Value[SFunc])
  extends Transformer[SCollection[IV], SCollection[OV]] {
  override def companion = MapCollection
  override val opCode: OpCode = OpCodes.MapCollectionCode
  implicit def tOV = mapper.asValue[OV].tpe
  override val tpe = SCollection[OV](mapper.tpe.tRange.asInstanceOf[OV])
  override val opType = SCollection.MapMethod.stype.asFunc
}
object MapCollection extends ValueCompanion {
}

case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Append
  override val opCode: OpCode = OpCodes.AppendCode
  override val tpe = input.tpe
  override val opType = SCollection.AppendMethod.stype.asFunc
}
object Append extends ValueCompanion {
}

case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Slice
  override val opCode: OpCode = OpCodes.SliceCode
  override val tpe = input.tpe
  override def opType = {
    val tpeColl = SCollection(input.tpe.typeParams.head.ident)
    SFunc(Vector(tpeColl, SInt, SInt), tpeColl)
  }
}
object Slice extends ValueCompanion {
}

case class Filter[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Filter
  override val opCode: OpCode = OpCodes.FilterCode
  override def tpe: SCollection[IV] = input.tpe
  override val opType = SCollection.FilterMethod.stype.asFunc
}
object Filter extends ValueCompanion {
}

trait BooleanTransformer[IV <: SType] extends Transformer[SCollection[IV], SBoolean.type] {
  override val input: Value[SCollection[IV]]
  val condition: Value[SFunc]
  override def tpe = SBoolean
}

case class Exists[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override def companion = Exists
  override val opCode: OpCode = OpCodes.ExistsCode
  override val opType = SCollection.ExistsMethod.stype.asFunc
}
object Exists extends ValueCompanion {
}

case class ForAll[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override def companion = ForAll
  override val opCode: OpCode = OpCodes.ForAllCode
  override val opType = SCollection.ForallMethod.stype.asFunc
}
object ForAll extends ValueCompanion {
}

case class Fold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                          zero: Value[OV],
                                          foldOp: Value[SFunc])
  extends Transformer[SCollection[IV], OV] {
  override def companion = Fold
  override val opCode: OpCode = OpCodes.FoldCode
  implicit override def tpe: OV = zero.tpe
  val opType: SFunc = SCollection.FoldMethod.stype.asFunc
}

object Fold extends ValueCompanion {
  def sum[T <: SNumericType](input: Value[SCollection[T]])(implicit tT: T) =
    Fold(input,
      Constant(tT.upcast(0.toByte), tT),
      FuncValue(Vector((1, STuple(tT, tT))),
        Plus(
          SelectField(ValUse(1, STuple(tT, tT)), 1).asNumValue,
          SelectField(ValUse(1, STuple(tT, tT)), 2).asNumValue))
    )

  def concat[T <: SType](input: Value[SCollection[SCollection[T]]])(implicit tT: T): Fold[SCollection[T], T] = {
    val tColl = SCollection(tT)
    Fold[SCollection[T], T](input,
      ConcreteCollection()(tT).asValue[T],
      FuncValue(Vector((1, tColl), (2, tColl)), Append(ValUse(1, tColl), ValUse(2, tColl)))
    )
  }
}

case class ByIndex[V <: SType](input: Value[SCollection[V]],
                               index: Value[SInt.type],
                               default: Option[Value[V]] = None)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] {
  override def companion = ByIndex
  override val opCode: OpCode = OpCodes.ByIndexCode
  override val tpe = input.tpe.elemType
  override val opType = SCollection.ApplyMethod.stype.asFunc
}
object ByIndex extends ValueCompanion {
}

/** Select tuple field by its 1-based index. E.g. input._1 is transformed to SelectField(input, 1) */
case class SelectField(input: Value[STuple], fieldIndex: Byte)
  extends Transformer[STuple, SType] with NotReadyValue[SType] {
  override def companion = SelectField
  override val opCode: OpCode = OpCodes.SelectFieldCode
  override val tpe = input.tpe.items(fieldIndex - 1)
  override val opType = SFunc(input.tpe, tpe)
}
object SelectField extends ValueCompanion {
}

/** Represents execution of Sigma protocol that validates the given input SigmaProp. */
case class SigmaPropIsProven(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SBoolean.type] with NotReadyValueBoolean {
  override def companion = SigmaPropIsProven
  override val opCode: OpCode = OpCodes.SigmaPropIsProvenCode
  override def opType = SFunc(input.tpe, SBoolean)
}
object SigmaPropIsProven extends ValueCompanion {
}

/** Extract serialized bytes of a SigmaProp value */
case class SigmaPropBytes(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SByteArray] with NotReadyValue[SByteArray] {
  override def companion = SigmaPropBytes
  override val opCode: OpCode = OpCodes.SigmaPropBytesCode
  override def tpe = SByteArray
  override val opType = SFunc(input.tpe, tpe)
}
object SigmaPropBytes extends ValueCompanion {
}

case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {
  override def companion = SizeOf
  override val opCode: OpCode = OpCodes.SizeOfCode
  override val opType = SFunc(SCollection(SCollection.tIV), SInt)
}
object SizeOf extends ValueCompanion {
}

sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
}

case class ExtractAmount(input: Value[SBox.type]) extends Extract[SLong.type] with NotReadyValueLong {
  override def companion = ExtractAmount
  override val opCode: OpCode = OpCodes.ExtractAmountCode
  override val opType = SFunc(SBox, SLong)
}
object ExtractAmount extends ValueCompanion {
}

case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractScriptBytes
  override val opCode: OpCode = OpCodes.ExtractScriptBytesCode
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractScriptBytes extends ValueCompanion {
}

case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractBytes
  override val opCode: OpCode = OpCodes.ExtractBytesCode
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractBytes extends ValueCompanion {
}

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractBytesWithNoRef
  override val opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractBytesWithNoRef extends ValueCompanion {
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractId
  override val opCode: OpCode = OpCodes.ExtractIdCode
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractId extends ValueCompanion {
}

case class ExtractRegisterAs[V <: SType]( input: Value[SBox.type],
                                          registerId: RegisterId,
                                          override val tpe: SOption[V])
  extends Extract[SOption[V]] with NotReadyValue[SOption[V]] {
  override def companion = ExtractRegisterAs
  override val opCode: OpCode = OpCodes.ExtractRegisterAs
  override def opType = SFunc(Vector(SBox, SByte), tpe)
}
object ExtractRegisterAs extends ValueCompanion {
  def apply[V <: SType](input: Value[SBox.type],
                        registerId: RegisterId)(implicit tpe: V): ExtractRegisterAs[V] =
    ExtractRegisterAs(input, registerId, SOption(tpe))
}

/**
  * Tuple of height when block got included into the blockchain and transaction identifier with box index in the transaction outputs serialized to the byte array.
  * @param input box
  */
case class ExtractCreationInfo(input: Value[SBox.type]) extends Extract[STuple] with NotReadyValue[STuple] {
  import ExtractCreationInfo._
  override def companion = ExtractCreationInfo
  override def tpe: STuple = ResultType
  override val opCode: OpCode = OpCodes.ExtractCreationInfoCode
  override def opType = OpType
}
object ExtractCreationInfo extends ValueCompanion {
  val ResultType = STuple(SInt, SByteArray)
  val OpType = SFunc(SBox, ResultType)
}

trait Deserialize[V <: SType] extends NotReadyValue[V]

/** Extracts context variable as Coll[Byte], deserializes it to script and then executes this script in the current context.
  * The original `Coll[Byte]` of the script is available as `getVar[Coll[Byte]](id)`
  * @param id identifier of the context variable
  * @tparam V result type of the deserialized script.
  * @throws InterpreterException if the actual script type doesn't conform to T
  * @return result of the script execution in the current context
  * @since 2.0
  */
case class DeserializeContext[V <: SType](id: Byte, tpe: V) extends Deserialize[V] {
  override def companion = DeserializeContext
  override val opCode: OpCode = OpCodes.DeserializeContextCode
  override val opType = SFunc(Vector(SContext, SByte), tpe)
}
object DeserializeContext extends ValueCompanion {
}

//todo: make it method of SBox and write test for this class
case class DeserializeRegister[V <: SType](reg: RegisterId, tpe: V, default: Option[Value[V]] = None) extends Deserialize[V] {
  override def companion = DeserializeRegister
  override val opCode: OpCode = OpCodes.DeserializeRegisterCode
  override val opType = SFunc(Vector(SBox, SByte, SOption(tpe)), tpe)
}
object DeserializeRegister extends ValueCompanion {
}

case class GetVar[V <: SType](varId: Byte, override val tpe: SOption[V]) extends NotReadyValue[SOption[V]] {
  override def companion = GetVar
  override val opCode: OpCode = OpCodes.GetVarCode
  override val opType = SFunc(Vector(SContext, SByte), tpe)
}
object GetVar extends ValueCompanion {
  def apply[V <: SType](varId: Byte, innerTpe: V): GetVar[V] = GetVar[V](varId, SOption(innerTpe))
}

case class OptionGet[V <: SType](input: Value[SOption[V]]) extends Transformer[SOption[V], V] {
  override def companion = OptionGet
  override val opCode: OpCode = OpCodes.OptionGetCode
  override val opType = SFunc(input.tpe, tpe)
  override def tpe: V = input.tpe.elemType
  override def toString: String = s"$input.get"
}
object OptionGet extends ValueCompanion {
}

case class OptionGetOrElse[V <: SType](input: Value[SOption[V]], default: Value[V])
  extends Transformer[SOption[V], V] {
  override def companion = OptionGetOrElse
  override val opCode: OpCode = OpCodes.OptionGetOrElseCode
  override val opType = SFunc(IndexedSeq(input.tpe, tpe), tpe)
  override def tpe: V = input.tpe.elemType
}
object OptionGetOrElse extends ValueCompanion {
}

case class OptionIsDefined[V <: SType](input: Value[SOption[V]])
  extends Transformer[SOption[V], SBoolean.type] {
  override def companion = OptionIsDefined
  override val opCode: OpCode = OpCodes.OptionIsDefinedCode
  override val opType = SFunc(input.tpe, SBoolean)
  override def tpe= SBoolean
}
object OptionIsDefined extends ValueCompanion {
}
