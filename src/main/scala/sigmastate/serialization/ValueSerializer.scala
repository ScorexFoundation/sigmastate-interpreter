package sigmastate.serialization

import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.exceptions.{InputSizeLimitExceeded, InvalidOpCode, ValueDeserializeCallDepthExceeded}
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupleSerializer, Relation2Serializer, Relation3Serializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter, SparseArrayContainer}


trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  override val companion = ValueSerializer

  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  val opCode: OpCode

}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  private val builder: DeserializationSigmaBuilder.type = DeserializationSigmaBuilder
  import builder._

  private val serializers = SparseArrayContainer.buildFrom(Seq[ValueSerializer[_ <: Value[SType]]](
    TupleSerializer(mkTuple),
    SelectFieldSerializer(mkSelectField),
    Relation2Serializer(GtCode, mkGT[SType]),
    Relation2Serializer(GeCode, mkGE[SType]),
    Relation2Serializer(LtCode, mkLT[SType]),
    Relation2Serializer(LeCode, mkLE[SType]),
    Relation2Serializer(EqCode, mkEQ[SType]),
    Relation2Serializer(NeqCode, mkNEQ[SType]),
    QuadrupleSerializer(TreeLookupCode, mkTreeLookup),
    QuadrupleSerializer(TreeModificationsCode, mkTreeModifications),
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
    TwoArgumentsSerializer(StringConcatCode, mkStringConcat),
    ProveDiffieHellmanTupleSerializer(mkProveDiffieHellmanTuple),
    ProveDlogSerializer(mkProveDlog),
    CaseObjectSerialization(TrueCode, TrueLeaf),
    CaseObjectSerialization(FalseCode, FalseLeaf),
    SigmaPropIsValidSerializer,
    SigmaPropBytesSerializer,
    ConcreteCollectionBooleanConstantSerializer(mkConcreteCollection),
    CaseObjectSerialization(HeightCode, Height),
    CaseObjectSerialization(InputsCode, Inputs),
    CaseObjectSerialization(OutputsCode, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHashCode, LastBlockUtxoRootHash),
    CaseObjectSerialization(SelfCode, Self),
    CaseObjectSerialization(GroupGeneratorCode, GroupGenerator),
    ConcreteCollectionSerializer(mkConcreteCollection),
    LogicalTransformerSerializer(AndCode, mkAND),
    LogicalTransformerSerializer(OrCode, mkOR),
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
    SimpleTransformerSerializer[SLong.type, SByteArray](LongToByteArrayCode, mkLongToByteArray),
    SimpleTransformerSerializer[SByteArray, SBigInt.type](ByteArrayToBigIntCode, mkByteArrayToBigInt),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcBlake2b256Code, mkCalcBlake2b256),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcSha256Code, mkCalcSha256),
    SimpleTransformerSerializer[SString.type, SByteArray](Base58ToByteArrayCode, mkBase58ToByteArray),
    SimpleTransformerSerializer[SString.type, SByteArray](Base64ToByteArrayCode, mkBase64ToByteArray),
    SimpleTransformerSerializer[SString.type, SSigmaProp.type](ErgoAddressToSigmaPropCode, mkPK),
    SimpleTransformerSerializer[SOption[SType], SType](OptionGetCode, mkOptionGet),
    SimpleTransformerSerializer[SOption[SType], SBoolean.type](OptionIsDefinedCode, mkOptionIsDefined),
    OptionGetOrElseSerializer(mkOptionGetOrElse),
    DeserializeContextSerializer(mkDeserializeContext),
    DeserializeRegisterSerializer(mkDeserializeRegister),
    ExtractRegisterAsSerializer(mkExtractRegisterAs),
    WhereSerializer(mkWhere),
    SliceSerializer(mkSlice),
    AtLeastSerializer(mkAtLeast),
    ByIndexSerializer(mkByIndex),
    AppendSerializer(mkAppend),
    NumericCastSerializer(UpcastCode, mkUpcast),
    NumericCastSerializer(DowncastCode, mkDowncast),
  ))

  private def serializable(v: Value[SType]): Value[SType] = v match {
    case upcast: Upcast[SType, _]@unchecked =>
      upcast.input
    case _ => v
  }

  override def getSerializer(opCode: Tag): ValueSerializer[_ <: Value[SType]] = {
    val serializer = serializers.get(opCode)
    if (serializer == null)
      throw new InvalidOpCode(s"Cannot find serializer for Value with opCode=$opCode")
    serializer
  }

  private val ConstantPlaceholderIndexCode = ConstantCode

  override def serialize(v: Value[SType], w: ByteWriter): Unit = serializable(v) match {
    case c: Constant[SType] =>
      w.payload[SerializedConstantPlaceholderStore] match {
        case Some(store) =>
          w.put(ConstantPlaceholderIndexCode)
            .putUInt(store.getIndex(c))
        case None =>
          ConstantSerializer(builder).serialize(c, w)
      }
    case _ =>
      val opCode = v.opCode
      w.put(opCode)
      // help compiler recognize the type
      getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]].serializeBody(v, w)
  }

  override def deserialize(r: ByteReader): Value[SType] = {
    val bytesRemaining = r.remaining
    if (bytesRemaining > Serializer.MaxInputSize)
      throw new InputSizeLimitExceeded(s"input size $bytesRemaining exceeds ${ Serializer.MaxInputSize}")
    val depth = r.level
    if (depth > Serializer.MaxTreeDepth)
      throw new ValueDeserializeCallDepthExceeded(s"nested value deserialization call depth($depth) exceeds allowed maximum ${Serializer.MaxTreeDepth}")
    r.level = depth + 1
    val firstByte = r.peekByte()
    val v = if (firstByte.toUByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      r.payload[SerializedConstantPlaceholderStore] match {
        case Some(store) =>
          if (r.getByte() == ConstantPlaceholderIndexCode) {
            store.getConstant(r.getUInt().toIntExact)
          } else {
            throw new IllegalStateException("Invalid constant placeholder code")
          }
        case None =>
          ConstantSerializer(builder).deserialize(r)
      }
    }
    else {
      val opCode = r.getByte()
      getSerializer(opCode).parseBody(r)
    }
    r.level = depth - 1
    v
  }

  private val SerializedConstantsStartCode = ConstantCode

  def serialize(v: Value[SType]): Array[Byte] = {
    val w = Serializer.startWriter()
    val constantPlaceholderStore = new SerializedConstantPlaceholderStore(builder)
    w.payload = constantPlaceholderStore
    serialize(v, w)
    val cw = Serializer.startWriter()
    // start new writer and put serialized constant first
    if (constantPlaceholderStore.nonEmpty) {
      cw.put(SerializedConstantsStartCode)
      constantPlaceholderStore.serialize(cw)
    }
    // add serialized tree
    cw.putBytes(w.toBytes)
    cw.toBytes
  }

  def deserialize(bytes: Array[Byte], pos: Serializer.Position = 0): Value[SType] = {
    val r = Serializer.startReader(bytes, pos)
    val firstByte = r.peekByte()
    if (firstByte == SerializedConstantsStartCode) {
      // consume
      r.getByte()
      r.payload = new SerializedConstantPlaceholderStore(builder).deserialize(r)
    }
    deserialize(r)
  }

}
