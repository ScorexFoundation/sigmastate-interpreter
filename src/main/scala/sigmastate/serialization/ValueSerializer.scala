package sigmastate.serialization

import sigmastate._
import Values._
import OpCodes._
import sigmastate.SCollection.SByteArray
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupleSerializer, Relation2Serializer, Relation3Serializer}
import sigmastate.utxo._
import sigmastate.utils.Extensions._
import org.ergoplatform._
import sigmastate.lang.DeserializationSigmaBuilder
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
    Relation3Serializer(IsMemberCode, mkIsMember),
    QuadrupleSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](IfCode, mkIf),
    TwoArgumentsSerializer(XorCode, mkXor),
    TwoArgumentsSerializer(ExponentiateCode, mkExponentiate),
    TwoArgumentsSerializer(MultiplyGroupCode, mkMultiplyGroup),
    TwoArgumentsSerializer(MinusCode, mkMinus[SNumericType]),
    TwoArgumentsSerializer(MultiplyCode, mkMultiply[SNumericType]),
    TwoArgumentsSerializer(DivisionCode, mkDivide[SNumericType]),
    TwoArgumentsSerializer(ModuloCode, mkModulo[SNumericType]),
    TwoArgumentsSerializer(PlusCode, mkPlus[SNumericType]),
    ProveDiffieHellmanTupleSerializer(mkProveDiffieHellmanTuple),
    ProveDlogSerializer,
    CaseObjectSerialization(TrueCode, TrueLeaf),
    CaseObjectSerialization(FalseCode, FalseLeaf),
    CaseObjectSerialization(HeightCode, Height),
    CaseObjectSerialization(InputsCode, Inputs),
    CaseObjectSerialization(OutputsCode, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHashCode, LastBlockUtxoRootHash),
    CaseObjectSerialization(SelfCode, Self),
    CaseObjectSerialization(GroupGeneratorCode, GroupGenerator),
    ConcreteCollectionSerializer,
    ConcreteCollectionBooleanConstantSerializer,
    LogicalTransformerSerializer(AndCode, mkAND),
    LogicalTransformerSerializer(OrCode, mkOR),
    TaggedVariableSerializer,
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
    SimpleTransformerSerializer[SInt.type, SByte.type](IntToByteCode, mkIntToByte),
    SimpleTransformerSerializer[SLong.type, SByteArray](LongToByteArrayCode, mkLongToByteArray),
    SimpleTransformerSerializer[SByteArray, SBigInt.type](ByteArrayToBigIntCode, mkByteArrayToBigInt),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcBlake2b256Code, mkCalcBlake2b256),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcSha256Code, mkCalcSha256),
    DeserializeContextSerializer(mkDeserializeContext),
    DeserializeRegisterSerializer(mkDeserializeRegister),
    ExtractRegisterAsSerializer(mkExtractRegisterAs),
    WhereSerializer(mkWhere),
    SliceSerializer(mkSlice),
    ByIndexSerializer(mkByIndex),
    AppendSerializer(mkAppend),
    UpcastSerializer,
  ))

  private def serializable(v: Value[SType]): Value[SType] = v match {
    case upcast: Upcast[SType, _]@unchecked =>
      upcast.input
    case _ => v
  }

  override def getSerializer(opCode: Tag): ValueSerializer[_ <: Value[SType]] = {
    val serializer = serializers.get(opCode)
    if (serializer == null) sys.error(s"Cannot find serializer for Value with opCode=$opCode")
    serializer
  }

  override def serialize(v: Value[SType], w: ByteWriter): Unit = serializable(v) match {
    case c: Constant[SType] =>
      ConstantSerializer.serialize(c, w)
    case _ =>
      val opCode = v.opCode
      w.put(opCode)
      // help compiler recognize the type
      getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]].serializeBody(v, w)
  }

  override def deserialize(r: ByteReader): Value[SType] = {
    val firstByte = r.peekByte()
    if (firstByte.toUByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      ConstantSerializer.deserialize(r)
    }
    else {
      val opCode = r.getByte()
      getSerializer(opCode).parseBody(r)
    }
  }

  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1
}
