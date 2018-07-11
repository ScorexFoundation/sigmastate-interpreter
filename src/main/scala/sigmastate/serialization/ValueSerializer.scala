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
import sigmastate.utils.{ByteReaderSigmaValues, ByteWriterSigmaValues, SparseArrayContainer}


trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  override val companion = ValueSerializer

  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  val opCode: OpCode

}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  private val serializers = SparseArrayContainer.buildFrom(Seq[ValueSerializer[_ <: Value[SType]]](
    TupleSerializer,
    SelectFieldSerializer,
    Relation2Serializer(GtCode, DeserializationSigmaBuilder.GT[SType]),
    Relation2Serializer(GeCode, DeserializationSigmaBuilder.GE[SType]),
    Relation2Serializer(LtCode, DeserializationSigmaBuilder.LT[SType]),
    Relation2Serializer(LeCode, DeserializationSigmaBuilder.LE[SType]),
    Relation2Serializer(EqCode, DeserializationSigmaBuilder.EQ[SType]),
    Relation2Serializer(NeqCode, DeserializationSigmaBuilder.NEQ[SType]),
    Relation3Serializer(IsMemberCode, IsMember.apply),
    QuadrupleSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](IfCode, If.apply),
    TwoArgumentsSerializer(XorCode, Xor.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply),
    TwoArgumentsSerializer(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializer(MinusCode, DeserializationSigmaBuilder.Minus[SNumericType]),
    TwoArgumentsSerializer(MultiplyCode, DeserializationSigmaBuilder.Multiply[SNumericType]),
    TwoArgumentsSerializer(DivisionCode, DeserializationSigmaBuilder.Divide[SNumericType]),
    TwoArgumentsSerializer(ModuloCode, DeserializationSigmaBuilder.Modulo[SNumericType]),
    TwoArgumentsSerializer(PlusCode, DeserializationSigmaBuilder.Plus[SNumericType]),
    ProveDiffieHellmanTupleSerializer,
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
    LogicalTransformerSerializer(AndCode, AND.apply),
    LogicalTransformerSerializer(OrCode, OR.apply),
    TaggedVariableSerializer,
    MapCollectionSerializer,
    BooleanTransformerSerializer[SType, BooleanTransformer[SType]](ExistsCode, Exists.apply),
    BooleanTransformerSerializer[SType, BooleanTransformer[SType]](ForAllCode, ForAll.apply),
    FoldSerializer,
    SimpleTransformerSerializer[SCollection[SType], SInt.type](SizeOfCode, SizeOf.apply),
    SimpleTransformerSerializer[SBox.type, SLong.type](ExtractAmountCode, ExtractAmount.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractScriptBytesCode, ExtractScriptBytes.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesCode, ExtractBytes.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesWithNoRefCode, ExtractBytesWithNoRef.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractIdCode, ExtractId.apply),
    SimpleTransformerSerializer[SInt.type, SByte.type](IntToByteCode, IntToByte.apply),
    SimpleTransformerSerializer[SLong.type, SByteArray](LongToByteArrayCode, LongToByteArray.apply),
    SimpleTransformerSerializer[SByteArray, SBigInt.type](ByteArrayToBigIntCode, ByteArrayToBigInt.apply),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcBlake2b256Code, CalcBlake2b256.apply),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcSha256Code, CalcSha256.apply),
    DeserializeContextSerializer,
    DeserializeRegisterSerializer,
    ExtractRegisterAsSerializer,
    WhereSerializer,
    SliceSerializer,
    ByIndexSerializer,
    AppendSerializer,
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

  override def serialize(v: Value[SType], w: ByteWriterSigmaValues): Unit = serializable(v) match {
    case c: Constant[SType] =>
      ConstantSerializer.serialize(c, w)
    case _ =>
      val opCode = v.opCode
      w.put(opCode)
      // help compiler recognize the type
      getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]].serializeBody(v, w)
  }

  override def deserialize(r: ByteReaderSigmaValues): Value[SType] = {
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
