package sigmastate.utxo

import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import org.ergoplatform.ErgoBox.{R3, RegisterId}
import sigmastate.Operations._
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
  implicit def tOV = mapper.asValue[OV].tpe
  override val tpe = SCollection[OV](mapper.tpe.tRange.asInstanceOf[OV])
  override val opType = SCollection.MapMethod.stype.asFunc
}
object MapCollection extends ValueCompanion {
  override def opCode: OpCode = OpCodes.MapCollectionCode
}

case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Append
  override val tpe = input.tpe
  override val opType = SCollection.AppendMethod.stype
}
object Append extends ValueCompanion {
  override def opCode: OpCode = OpCodes.AppendCode
}

case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Slice
  override val tpe = input.tpe
  override def opType = {
    val tpeColl = SCollection(input.tpe.typeParams.head.ident)
    SFunc(Vector(tpeColl, SInt, SInt), tpeColl)
  }
}
object Slice extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SliceCode
}

case class Filter[IV <: SType](input: Value[SCollection[IV]],
                               condition: Value[SFunc])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Filter
  override def tpe: SCollection[IV] = input.tpe
  override val opType = SCollection.FilterMethod.stype
}
object Filter extends ValueCompanion {
  override def opCode: OpCode = OpCodes.FilterCode
}

trait BooleanTransformer[IV <: SType] extends Transformer[SCollection[IV], SBoolean.type] {
  override val input: Value[SCollection[IV]]
  val condition: Value[SFunc]
  override def tpe = SBoolean
}
trait BooleanTransformerCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

case class Exists[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override def companion = Exists
  override val opType = SCollection.ExistsMethod.stype
}
object Exists extends BooleanTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExistsCode
  override def argInfos: Seq[ArgInfo] = ExistsInfo.argInfos
}

case class ForAll[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override def companion = ForAll
  override val opType = SCollection.ForallMethod.stype
}
object ForAll extends BooleanTransformerCompanion {
  override def opCode: OpCode = OpCodes.ForAllCode
  override def argInfos: Seq[ArgInfo] = ForAllInfo.argInfos
}

case class Fold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                          zero: Value[OV],
                                          foldOp: Value[SFunc])
  extends Transformer[SCollection[IV], OV] {
  override def companion = Fold
  implicit override def tpe: OV = zero.tpe
  val opType: SFunc = SCollection.FoldMethod.stype
}

object Fold extends ValueCompanion {
  override def opCode: OpCode = OpCodes.FoldCode
  def sum[T <: SNumericType](input: Value[SCollection[T]], varId: Int)(implicit tT: T) =
    Fold(input,
      Constant(tT.upcast(0.toByte), tT),
      FuncValue(Vector((varId, STuple(tT, tT))),
        Plus(
          SelectField(ValUse(varId, STuple(tT, tT)), 1).asNumValue,
          SelectField(ValUse(varId, STuple(tT, tT)), 2).asNumValue))
    )

  def concat[T <: SType](input: Value[SCollection[SCollection[T]]])(implicit tT: T): Fold[SCollection[T], T] = {
    val tColl = SCollection(tT)
    Fold[SCollection[T], T](input,
      ConcreteCollection(Array[Value[T]](), tT).asValue[T],
      FuncValue(Array((1, tColl), (2, tColl)), Append(ValUse(1, tColl), ValUse(2, tColl)))
    )
  }
}

case class ByIndex[V <: SType](input: Value[SCollection[V]],
                               index: Value[SInt.type],
                               default: Option[Value[V]] = None)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] {
  override def companion = ByIndex
  override val tpe = input.tpe.elemType
  override val opType = SCollection.ApplyMethod.stype.asFunc
}
object ByIndex extends ValueCompanion {
  override def opCode: OpCode = OpCodes.ByIndexCode
}

/** Select tuple field by its 1-based index. E.g. input._1 is transformed to SelectField(input, 1) */
case class SelectField(input: Value[STuple], fieldIndex: Byte)
  extends Transformer[STuple, SType] with NotReadyValue[SType] {
  override def companion = SelectField
  override val tpe = input.tpe.items(fieldIndex - 1)
  override val opType = SFunc(input.tpe, tpe)
}
object SelectField extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SelectFieldCode
  def typed[T <: SValue](input: Value[STuple], fieldIndex: Byte): T = {
    SelectField(input, fieldIndex).asInstanceOf[T]
  }
}

/** Represents execution of Sigma protocol that validates the given input SigmaProp. */
case class SigmaPropIsProven(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SBoolean.type] with NotReadyValueBoolean {
  override def companion = SigmaPropIsProven
  override def opType = SFunc(input.tpe, SBoolean)
}
object SigmaPropIsProven extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SigmaPropIsProvenCode
}

/** Extract serialized bytes of a SigmaProp value */
case class SigmaPropBytes(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SByteArray] with NotReadyValue[SByteArray] {
  override def companion = SigmaPropBytes
  override def tpe = SByteArray
  override val opType = SFunc(input.tpe, tpe)
}
object SigmaPropBytes extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SigmaPropBytesCode
}
trait SimpleTransformerCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}
case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {
  override def companion = SizeOf
  override val opType = SFunc(SCollection(SCollection.tIV), SInt)
}
object SizeOf extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.SizeOfCode
  override def argInfos: Seq[ArgInfo] = SizeOfInfo.argInfos
}

sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
}

case class ExtractAmount(input: Value[SBox.type]) extends Extract[SLong.type] with NotReadyValueLong {
  override def companion = ExtractAmount
  override val opType = SFunc(SBox, SLong)
}
object ExtractAmount extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractAmountCode
  override def argInfos: Seq[ArgInfo] = ExtractAmountInfo.argInfos
}

case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractScriptBytes
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractScriptBytes extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractScriptBytesCode
  override def argInfos: Seq[ArgInfo] = ExtractScriptBytesInfo.argInfos
}

case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractBytes
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractBytes extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractBytesCode
  override def argInfos: Seq[ArgInfo] = ExtractBytesInfo.argInfos
}

case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractBytesWithNoRef
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractBytesWithNoRef extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode
  override def argInfos: Seq[ArgInfo] = ExtractBytesWithNoRefInfo.argInfos
}

case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractId
  override val opType = SFunc(SBox, SByteArray)
}
object ExtractId extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractIdCode
  override def argInfos: Seq[ArgInfo] = ExtractIdInfo.argInfos
}

case class ExtractRegisterAs[V <: SType]( input: Value[SBox.type],
                                          registerId: RegisterId,
                                          override val tpe: SOption[V])
  extends Extract[SOption[V]] with NotReadyValue[SOption[V]] {
  override def companion = ExtractRegisterAs
  override def opType = SFunc(Vector(SBox, SByte), tpe)
}
object ExtractRegisterAs extends ValueCompanion {
  override def opCode: OpCode = OpCodes.ExtractRegisterAs
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
  override def opType = OpType
}
object ExtractCreationInfo extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractCreationInfoCode
  override def argInfos: Seq[ArgInfo] = ExtractCreationInfoInfo.argInfos
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
  override val opType = SFunc(Vector(SContext, SByte), tpe)
}
object DeserializeContext extends ValueCompanion {
  override def opCode: OpCode = OpCodes.DeserializeContextCode
}

/** Extract register of SELF box as Coll[Byte], deserialize it into Value and inline into executing script.
  * NOTE: it only applicable to SELF box
  */
case class DeserializeRegister[V <: SType](reg: RegisterId, tpe: V, default: Option[Value[V]] = None) extends Deserialize[V] {
  override def companion = DeserializeRegister
  override val opType = SFunc(Vector(SBox, SByte, SOption(tpe)), tpe)
}
object DeserializeRegister extends ValueCompanion {
  override def opCode: OpCode = OpCodes.DeserializeRegisterCode
}

case class GetVar[V <: SType](varId: Byte, override val tpe: SOption[V]) extends NotReadyValue[SOption[V]] {
  override def companion = GetVar
  override val opType = SFunc(Vector(SContext, SByte), tpe)
}
object GetVar extends ValueCompanion {
  override def opCode: OpCode = OpCodes.GetVarCode
  def apply[V <: SType](varId: Byte, innerTpe: V): GetVar[V] = GetVar[V](varId, SOption(innerTpe))
}

case class OptionGet[V <: SType](input: Value[SOption[V]]) extends Transformer[SOption[V], V] {
  override def companion = OptionGet
  override val opType = SFunc(input.tpe, tpe)
  override def tpe: V = input.tpe.elemType
  override def toString: String = s"$input.get"
}
object OptionGet extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.OptionGetCode
  override def argInfos: Seq[ArgInfo] = OptionGetInfo.argInfos
}

case class OptionGetOrElse[V <: SType](input: Value[SOption[V]], default: Value[V])
  extends Transformer[SOption[V], V] {
  override def companion = OptionGetOrElse
  override val opType = SFunc(IndexedSeq(input.tpe, tpe), tpe)
  override def tpe: V = input.tpe.elemType
}
object OptionGetOrElse extends ValueCompanion {
  override def opCode: OpCode = OpCodes.OptionGetOrElseCode
}

case class OptionIsDefined[V <: SType](input: Value[SOption[V]])
  extends Transformer[SOption[V], SBoolean.type] {
  override def companion = OptionIsDefined
  override val opType = SFunc(input.tpe, SBoolean)
  override def tpe= SBoolean
}
object OptionIsDefined extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.OptionIsDefinedCode
  override def argInfos: Seq[ArgInfo] = OptionIsDefinedInfo.argInfos
}
