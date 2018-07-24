package sigmastate.serialization

import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupleSerializer, Relation2Serializer, Relation3Serializer}
import sigmastate.utils.Extensions._
import org.ergoplatform._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.exceptions.{InvalidOpCode, ValueDeserializeCallDepthExceeded}
import sigmastate.utils.{ByteReader, ByteWriter, SparseArrayContainer}

import scala.collection.concurrent.TrieMap


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
    ProveDlogSerializer(mkProveDlog),
    CaseObjectSerialization(TrueCode, TrueLeaf),
    CaseObjectSerialization(FalseCode, FalseLeaf),
    CaseObjectSerialization(HeightCode, Height),
    CaseObjectSerialization(InputsCode, Inputs),
    CaseObjectSerialization(OutputsCode, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHashCode, LastBlockUtxoRootHash),
    CaseObjectSerialization(SelfCode, Self),
    CaseObjectSerialization(GroupGeneratorCode, GroupGenerator),
    ConcreteCollectionSerializer(mkConcreteCollection),
    ConcreteCollectionBooleanConstantSerializer(mkConcreteCollection),
    LogicalTransformerSerializer(AndCode, mkAND),
    LogicalTransformerSerializer(OrCode, mkOR),
    TaggedVariableSerializer(mkTaggedVariable),
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
    UpcastSerializer(mkUpcast),
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

  override def serialize(v: Value[SType], w: ByteWriter): Unit = serializable(v) match {
    case c: Constant[SType] =>
      ConstantSerializer(builder).serialize(c, w)
    case _ =>
      val opCode = v.opCode
      w.put(opCode)
      // help compiler recognize the type
      getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]].serializeBody(v, w)
  }

  private val nestedValuesDepthPerReader = TrieMap[Int, Int]()

  override def deserialize(r: ByteReader): Value[SType] = {
    val depthKey = r.hashCode()
    val depth = nestedValuesDepthPerReader.getOrElseUpdate(depthKey, 0)
    if (depth > Serializer.MaxTreeDepth)
      throw new ValueDeserializeCallDepthExceeded(s"nested value deserialization call depth($depth) exceeds allowed maximum ${Serializer.MaxTreeDepth}")
    nestedValuesDepthPerReader.update(depthKey, depth + 1)
    val firstByte = r.peekByte()
    val v = if (firstByte.toUByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      ConstantSerializer(builder).deserialize(r)
    }
    else {
      val opCode = r.getByte()
      getSerializer(opCode).parseBody(r)
    }
    if (depth == 1)
      nestedValuesDepthPerReader.remove(depthKey)
    else
      nestedValuesDepthPerReader.update(depthKey, depth - 1)
    v
  }

  def serialize(v: Value[SType]): Array[Byte] = {
    val w = Serializer.startWriter()
    serialize(v, w)
    w.toBytes
  }

  def deserialize(bytes: Array[Byte], pos: Serializer.Position = 0): Value[SType] =
    deserialize(Serializer.startReader(bytes, pos))

}
