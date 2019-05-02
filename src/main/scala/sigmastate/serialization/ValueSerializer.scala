package sigmastate.serialization

import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.Terms.OperationId
import sigmastate.lang.exceptions.{InputSizeLimitExceeded, InvalidOpCode, ValueDeserializeCallDepthExceeded}
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupleSerializer, Relation2Serializer}
import sigma.util.Extensions._
import sigmastate.utils._
import sigmastate.utxo.CostTable._

trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  val companion = ValueSerializer

  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  val opCode: OpCode

  def opCost(opId: OperationId): ExpressionCost =
    sys.error(s"Operation opCost is not defined for AST node ${this.getClass}")
}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  private val builder: DeserializationSigmaBuilder.type = DeserializationSigmaBuilder
  import builder._

  private val constantSerializer = ConstantSerializer(builder)
  private val constantPlaceholderSerializer = ConstantPlaceholderSerializer(mkConstantPlaceholder)

  private val serializers = SparseArrayContainer.buildForSerializers(Seq[ValueSerializer[_ <: Value[SType]]](
    constantSerializer,
    constantPlaceholderSerializer,
    TupleSerializer(mkTuple),
    SelectFieldSerializer(mkSelectField),
    Relation2Serializer(GtCode, mkGT[SType]),
    Relation2Serializer(GeCode, mkGE[SType]),
    Relation2Serializer(LtCode, mkLT[SType]),
    Relation2Serializer(LeCode, mkLE[SType]),
    Relation2Serializer(EqCode, mkEQ[SType]),
    Relation2Serializer(NeqCode, mkNEQ[SType]),
    CreateAvlTreeSerializer(mkCreateAvlTree),
    QuadrupleSerializer(AvlTreeGetCode, mkTreeLookup),
//    QuadrupleSerializer(TreeUpdatesCode, mkTreeUpdates),
//    QuadrupleSerializer(TreeInsertsCode, mkTreeInserts),
//    QuadrupleSerializer(TreeRemovalsCode, mkTreeRemovals),
    Relation2Serializer(BinOrCode, mkBinOr),
    Relation2Serializer(BinAndCode, mkBinAnd),
    Relation2Serializer(BinXorCode, mkBinXor),
    QuadrupleSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](IfCode, mkIf),
    TwoArgumentsSerializer(XorCode, mkXor),
    TwoArgumentsSerializer(ExponentiateCode, mkExponentiate),
    TwoArgumentsSerializer(MultiplyGroupCode, mkMultiplyGroup),
    TwoArgumentsSerializer(MinusCode, mkMinus[SNumericType]),
    TwoArgumentsSerializer(MultiplyCode, mkMultiply[SNumericType]),
    TwoArgumentsSerializer(DivisionCode, mkDivide[SNumericType]),
    TwoArgumentsSerializer(ModuloCode, mkModulo[SNumericType]),
    TwoArgumentsSerializer(PlusCode, mkPlus[SNumericType]),
    TwoArgumentsSerializer(MinCode, mkMin[SNumericType]),
    TwoArgumentsSerializer(MaxCode, mkMax[SNumericType]),
    TwoArgumentsSerializer(BitOrCode, mkBitOr[SNumericType]),
    TwoArgumentsSerializer(BitAndCode, mkBitAnd[SNumericType]),
    TwoArgumentsSerializer(BitXorCode, mkBitXor[SNumericType]),
    TwoArgumentsSerializer(BitShiftLeftCode, mkBitShiftLeft[SNumericType]),
    TwoArgumentsSerializer(BitShiftRightCode, mkBitShiftRight[SNumericType]),
    TwoArgumentsSerializer(BitShiftRightZeroedCode, mkBitShiftRightZeroed[SNumericType]),
    CaseObjectSerialization(TrueCode, TrueLeaf),
    CaseObjectSerialization(FalseCode, FalseLeaf),
    SigmaPropIsProvenSerializer,
    SigmaPropBytesSerializer,
    ConcreteCollectionBooleanConstantSerializer(mkConcreteCollection),
    CaseObjectSerialization(ContextCode, Context),
    CaseObjectSerialization(GlobalCode, Global),
    CaseObjectSerialization(HeightCode, Height),
    CaseObjectSerialization(MinerPubkeyCode, MinerPubkey),
    CaseObjectSerialization(InputsCode, Inputs),
    CaseObjectSerialization(OutputsCode, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHashCode, LastBlockUtxoRootHash),
    CaseObjectSerialization(SelfCode, Self),
    CaseObjectSerialization(GroupGeneratorCode, GroupGenerator),
    ConcreteCollectionSerializer(mkConcreteCollection),
    LogicalTransformerSerializer(AndCode, mkAND),
    LogicalTransformerSerializer(OrCode, mkOR),
    LogicalTransformerSerializer(XorOfCode, mkXorOf),
    TaggedVariableSerializer(mkTaggedVariable),
    GetVarSerializer(mkGetVar),
    MapCollectionSerializer(mkMapCollection),
    BooleanTransformerSerializer[SType](ExistsCode, mkExists),
    BooleanTransformerSerializer[SType](ForAllCode, mkForAll),
    FoldSerializer(mkFold),
    SimpleTransformerSerializer[SCollection[SType], SInt.type](SizeOfCode, mkSizeOf),
    SimpleTransformerSerializer[SBox.type, SLong.type](ExtractAmountCode, mkExtractAmount),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractScriptBytesCode, mkExtractScriptBytes),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesCode, mkExtractBytes),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesWithNoRefCode, mkExtractBytesWithNoRef),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractIdCode, mkExtractId),
    SimpleTransformerSerializer[SBox.type, STuple](ExtractCreationInfoCode, mkExtractCreationInfo),
    SimpleTransformerSerializer[SLong.type, SByteArray](LongToByteArrayCode, mkLongToByteArray),
    SimpleTransformerSerializer[SByteArray, SLong.type](ByteArrayToLongCode, mkByteArrayToLong),
    SimpleTransformerSerializer[SByteArray, SBigInt.type](ByteArrayToBigIntCode, mkByteArrayToBigInt),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcBlake2b256Code, mkCalcBlake2b256),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcSha256Code, mkCalcSha256),
    SimpleTransformerSerializer[SByteArray, SGroupElement.type](DecodePointCode, mkDecodePoint),
    SimpleTransformerSerializer[SOption[SType], SType](OptionGetCode, mkOptionGet),
    SimpleTransformerSerializer[SOption[SType], SBoolean.type](OptionIsDefinedCode, mkOptionIsDefined),
    OptionGetOrElseSerializer(mkOptionGetOrElse),
    DeserializeContextSerializer(mkDeserializeContext),
    DeserializeRegisterSerializer(mkDeserializeRegister),
    ExtractRegisterAsSerializer(mkExtractRegisterAs),
    FilterSerializer(mkFilter),
    SliceSerializer(mkSlice),
    AtLeastSerializer(mkAtLeast),
    ByIndexSerializer(mkByIndex),
    AppendSerializer(mkAppend),
    NumericCastSerializer(UpcastCode, mkUpcast),
    NumericCastSerializer(DowncastCode, mkDowncast),
    ValDefSerializer(ValDefCode),
    ValDefSerializer(FunDefCode),
    BlockValueSerializer(mkBlockValue),
    ValUseSerializer(mkValUse),
    FuncValueSerializer(mkFuncValue),
    ApplySerializer(mkApply),
    MethodCallSerializer(PropertyCallCode, mkMethodCall),
    MethodCallSerializer(MethodCallCode, mkMethodCall),
    SigmaTransformerSerializer(SigmaAndCode, mkSigmaAnd),
    SigmaTransformerSerializer(SigmaOrCode, mkSigmaOr),
    BoolToSigmaPropSerializer(mkBoolToSigmaProp),
    ModQSerializer,
    ModQArithOpSerializer(PlusModQCode, mkPlusModQ),
    ModQArithOpSerializer(MinusModQCode, mkMinusModQ),
    SubstConstantsSerializer,
    CreateProveDlogSerializer(mkCreateProveDlog),
    CreateProveDHTupleSerializer(mkCreateProveDHTuple),
    LogicalNotSerializer(mkLogicalNot),
    OneArgumentOperationSerializer(NegationCode, mkNegation[SNumericType]),
    OneArgumentOperationSerializer(BitInversionCode, mkBitInversion[SNumericType]),
  ))

  private def serializable(v: Value[SType]): Value[SType] = v match {
    case upcast: Upcast[SType, _]@unchecked =>
      upcast.input
    case _ => v
  }

  override def getSerializer(opCode: Tag): ValueSerializer[_ <: Value[SType]] = {
    val serializer = serializers.get(opCode)
    if (serializer == null)
      throw new InvalidOpCode(s"Cannot find serializer for Value with opCode = LastConstantCode + ${opCode.toUByte - LastConstantCode}")
    serializer
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
      w.put(opCode)
      // help compiler recognize the type
      getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]].serialize(v, w)
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
    r.level = r.level - 1
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
