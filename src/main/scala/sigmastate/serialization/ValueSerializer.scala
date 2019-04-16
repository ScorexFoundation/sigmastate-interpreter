package sigmastate.serialization

import java.util.Properties

import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.Terms.{OperationId, PropertyCall, MethodCall}
import sigmastate.lang.exceptions.{InputSizeLimitExceeded, InvalidOpCode, ValueDeserializeCallDepthExceeded}
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupleSerializer, Relation2Serializer}
import sigma.util.Extensions._
import sigmastate.utils._
import sigmastate.utxo.CostTable._
import sigmastate.utxo._

import scala.collection.mutable

trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  val companion = ValueSerializer
  def opDesc: ValueCompanion

  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  def opCode: OpCode = opDesc.opCode

  def opCost(opId: OperationId): ExpressionCost =
    sys.error(s"Operation opCost is not defined for AST node ${this.getClass}")
}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  private val builder: DeserializationSigmaBuilder.type = DeserializationSigmaBuilder
  import builder._

  private val constantSerializer = ConstantSerializer(builder)
  private val constantPlaceholderSerializer = ConstantPlaceholderSerializer(mkConstantPlaceholder)

  val serializers = SparseArrayContainer.buildForSerializers(Seq[ValueSerializer[_ <: Value[SType]]](
    constantSerializer,
    constantPlaceholderSerializer,
    TupleSerializer(mkTuple),
    SelectFieldSerializer(mkSelectField),
    Relation2Serializer(GT, mkGT[SType]),
    Relation2Serializer(GE, mkGE[SType]),
    Relation2Serializer(LT, mkLT[SType]),
    Relation2Serializer(LE, mkLE[SType]),
    Relation2Serializer(EQ, mkEQ[SType]),
    Relation2Serializer(NEQ, mkNEQ[SType]),
    CreateAvlTreeSerializer(mkCreateAvlTree),
    QuadrupleSerializer(TreeLookup, mkTreeLookup),
    Relation2Serializer(BinOr, mkBinOr),
    Relation2Serializer(BinAnd, mkBinAnd),
    Relation2Serializer(BinXor, mkBinXor),
    QuadrupleSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](If, mkIf),
    TwoArgumentsSerializer(Xor, mkXor),
    TwoArgumentsSerializer(Exponentiate, mkExponentiate),
    TwoArgumentsSerializer(MultiplyGroup, mkMultiplyGroup),
    TwoArgumentsSerializer(ArithOp.Minus, mkMinus[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Multiply, mkMultiply[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Division, mkDivide[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Modulo, mkModulo[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Plus, mkPlus[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Min, mkMin[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Max, mkMax[SNumericType]),
    TwoArgumentsSerializer(BitOp.Or, mkBitOr[SNumericType]),
    TwoArgumentsSerializer(BitOp.And, mkBitAnd[SNumericType]),
    TwoArgumentsSerializer(BitOp.Xor, mkBitXor[SNumericType]),
    TwoArgumentsSerializer(BitOp.ShiftLeft, mkBitShiftLeft[SNumericType]),
    TwoArgumentsSerializer(BitOp.ShiftRight, mkBitShiftRight[SNumericType]),
    TwoArgumentsSerializer(BitOp.ShiftRightZeroed, mkBitShiftRightZeroed[SNumericType]),
    SigmaPropIsProvenSerializer,
    SigmaPropBytesSerializer,
    ConcreteCollectionBooleanConstantSerializer(mkConcreteCollection),
    CaseObjectSerialization(TrueLeaf, TrueLeaf),
    CaseObjectSerialization(FalseLeaf, FalseLeaf),
    CaseObjectSerialization(Context, Context),
    CaseObjectSerialization(Global, Global),
    CaseObjectSerialization(Height, Height),
    CaseObjectSerialization(MinerPubkey, MinerPubkey),
    CaseObjectSerialization(Inputs, Inputs),
    CaseObjectSerialization(Outputs, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHash, LastBlockUtxoRootHash),
    CaseObjectSerialization(Self, Self),
    CaseObjectSerialization(GroupGenerator, GroupGenerator),
    ConcreteCollectionSerializer(mkConcreteCollection),
    LogicalTransformerSerializer(AND, mkAND),
    LogicalTransformerSerializer(OR, mkOR),
    LogicalTransformerSerializer(XorOf, mkXorOf),
    TaggedVariableSerializer(mkTaggedVariable),
    GetVarSerializer(mkGetVar),
    MapCollectionSerializer(mkMapCollection),
    BooleanTransformerSerializer[SType](Exists, mkExists),
    BooleanTransformerSerializer[SType](ForAll, mkForAll),
    FoldSerializer(mkFold),
    SimpleTransformerSerializer[SCollection[SType], SInt.type](SizeOf, mkSizeOf),
    SimpleTransformerSerializer[SBox.type, SLong.type](ExtractAmount, mkExtractAmount),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractScriptBytes, mkExtractScriptBytes),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytes, mkExtractBytes),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesWithNoRef, mkExtractBytesWithNoRef),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractId, mkExtractId),
    SimpleTransformerSerializer[SBox.type, STuple](ExtractCreationInfo, mkExtractCreationInfo),
    SimpleTransformerSerializer[SLong.type, SByteArray](LongToByteArray, mkLongToByteArray),
    SimpleTransformerSerializer[SByteArray, SLong.type](ByteArrayToLong, mkByteArrayToLong),
    SimpleTransformerSerializer[SByteArray, SBigInt.type](ByteArrayToBigInt, mkByteArrayToBigInt),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcBlake2b256, mkCalcBlake2b256),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcSha256, mkCalcSha256),
    SimpleTransformerSerializer[SByteArray, SGroupElement.type](DecodePoint, mkDecodePoint),
    SimpleTransformerSerializer[SOption[SType], SType](OptionGet, mkOptionGet),
    SimpleTransformerSerializer[SOption[SType], SBoolean.type](OptionIsDefined, mkOptionIsDefined),
    OptionGetOrElseSerializer(mkOptionGetOrElse),
    DeserializeContextSerializer(mkDeserializeContext),
    DeserializeRegisterSerializer(mkDeserializeRegister),
    ExtractRegisterAsSerializer(mkExtractRegisterAs),
    FilterSerializer(mkFilter),
    SliceSerializer(mkSlice),
    AtLeastSerializer(mkAtLeast),
    ByIndexSerializer(mkByIndex),
    AppendSerializer(mkAppend),
    NumericCastSerializer(Upcast, mkUpcast),
    NumericCastSerializer(Downcast, mkDowncast),
    ValDefSerializer(ValDef),
    ValDefSerializer(FunDef),
    BlockValueSerializer(mkBlockValue),
    ValUseSerializer(mkValUse),
    FuncValueSerializer(mkFuncValue),
    ApplySerializer(mkApply),
    MethodCallSerializer(PropertyCall, mkMethodCall),
    MethodCallSerializer(MethodCall, mkMethodCall),
    SigmaTransformerSerializer(SigmaAnd, mkSigmaAnd),
    SigmaTransformerSerializer(SigmaOr, mkSigmaOr),
    BoolToSigmaPropSerializer(mkBoolToSigmaProp),
    ModQSerializer,
    ModQArithOpSerializer(ModQArithOp.Plus, mkPlusModQ),
    ModQArithOpSerializer(ModQArithOp.Minus, mkMinusModQ),
    SubstConstantsSerializer,
    CreateProveDlogSerializer(mkCreateProveDlog),
    CreateProveDHTupleSerializer(mkCreateProveDHTuple),
    LogicalNotSerializer(mkLogicalNot),
    OneArgumentOperationSerializer(Negation, mkNegation[SNumericType]),
    OneArgumentOperationSerializer(BitInversion, mkBitInversion[SNumericType]),
  ))

  private def serializable(v: Value[SType]): Value[SType] = v match {
    case upcast: Upcast[SType, _]@unchecked =>
      upcast.input
    case _ => v
  }

  override def getSerializer(opCode: Tag): ValueSerializer[_ <: Value[SType]] = {
    val serializer = serializers(opCode)
    if (serializer == null)
      throw new InvalidOpCode(s"Cannot find serializer for Value with opCode = LastConstantCode + ${opCode.toUByte - LastConstantCode}")
    serializer
  }

  case class SerInfo(opCode: OpCode, properties: Properties)

  val collectSerInfo: Boolean = true
  val serializerInfo: mutable.Map[OpCode, SerInfo] = mutable.HashMap.empty
  private var serializerStack: List[ValueSerializer[_]] = Nil

  def printSerInfo(): String = {
    serializerInfo.map { case (_, s) =>
      val ser = getSerializer(s.opCode)
      s"${ser.opDesc}: ${s.properties}"
    }.mkString("\n")
  }

  def addArgInfo(prop: PropInfo) = {
    val ser = serializerStack.head
    val info = serializerInfo(ser.opCode)
    val saved = info.properties.get(prop.name)
    if (saved == null) {
      info.properties.put(prop.name, prop)
      println(s"Added $prop to ${ser.opDesc}")
    }
    else {
      assert(saved == prop, s"Saved property $saved is different from being added $prop: operation ${ser.opDesc}")
    }
  }

  override def serialize(v: Value[SType], w: SigmaByteWriter): Unit = serializable(v) match {
    case c: Constant[SType] =>
      w.constantExtractionStore match {
        case Some(constantStore) =>
          val ph = constantStore.put(c)(DeserializationSigmaBuilder)
          w.put(ph.opCode)
          constantPlaceholderSerializer.serialize(ph, w)
        case None =>
          constantSerializer.serialize(c, w)
      }
    case _ =>
      val opCode = v.opCode
      // help compiler recognize the type
      val ser = getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]]
      if (collectSerInfo) {
        serializerInfo.get(opCode) match {
          case None =>
            serializerInfo += (opCode -> SerInfo(opCode, new Properties()))
            println(s"Added: ${ser.opDesc}")
          case _ =>
        }
      }
      w.put(opCode)

      serializerStack ::= ser
      ser.serialize(v, w)
      serializerStack = serializerStack.tail
  }

  override def deserialize(r: SigmaByteReader): Value[SType] = {
    val bytesRemaining = r.remaining
    if (bytesRemaining > SigmaSerializer.MaxInputSize)
      throw new InputSizeLimitExceeded(s"input size $bytesRemaining exceeds ${ SigmaSerializer.MaxInputSize}")
    val depth = r.level
    if (depth > SigmaSerializer.MaxTreeDepth)
      throw new ValueDeserializeCallDepthExceeded(s"nested value deserialization call depth($depth) exceeds allowed maximum ${SigmaSerializer.MaxTreeDepth}")
    r.level = depth + 1
    val firstByte = r.peekByte().toUByte
    val v = if (firstByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      constantSerializer.deserialize(r)
    }
    else {
      val opCode = r.getByte()
      getSerializer(opCode).parse(r)
    }
    r.level = depth - 1
    v
  }

  def serialize(v: Value[SType]): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serialize(v, w)
    w.toBytes
  }

  def deserialize(bytes: Array[Byte], pos: SigmaSerializer.Position = 0): Value[SType] =
    deserialize(SigmaSerializer.startReader(bytes, pos))

}
