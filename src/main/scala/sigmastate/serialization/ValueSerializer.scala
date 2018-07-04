package sigmastate.serialization

import sigmastate._
import Values._

import scala.util.Try
import OpCodes._
import sigmastate.SCollection.SByteArray
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupleSerializer, Relation2Serializer, Relation3Serializer}
import sigmastate.utxo._
import sigmastate.utils.Extensions._
import Serializer.Consumed
import javax.swing.SpringLayout.Constraints
import org.ergoplatform._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.utils.{ByteReader, ByteWriter}


trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  override val companion = ValueSerializer

  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  val opCode: OpCode

}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  val table: Map[OpCode, ValueSerializer[_ <: Value[SType]]] = Seq[ValueSerializer[_ <: Value[SType]]](

    Relation2Serializer(GtCode, DeserializationSigmaBuilder.GT[SType]),
    Relation2Serializer(GeCode, DeserializationSigmaBuilder.GE[SType]),
    Relation2Serializer(LtCode, DeserializationSigmaBuilder.LT[SType]),
    Relation2Serializer(LeCode, DeserializationSigmaBuilder.LE[SType]),
    Relation2Serializer(EqCode, DeserializationSigmaBuilder.EQ[SType]),
    Relation2Serializer(NeqCode, DeserializationSigmaBuilder.NEQ[SType]),
    Relation3Serializer(IsMemberCode, IsMember.apply),
    QuadrupelSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](IfCode, If.apply),

    TwoArgumentsSerializer(XorCode, Xor.apply),
//    TwoArgumentsSerializer(AppendBytesCode, AppendBytes.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply),
    TwoArgumentsSerializer(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializer(MinusCode, DeserializationSigmaBuilder.Minus[SNumericType]),
    TwoArgumentsSerializer(MultiplyCode, DeserializationSigmaBuilder.Multiply[SNumericType]),
    TwoArgumentsSerializer(DivisionCode, DeserializationSigmaBuilder.Divide[SNumericType]),
    TwoArgumentsSerializer(ModuloCode, DeserializationSigmaBuilder.Modulo[SNumericType]),
    TwoArgumentsSerializer(PlusCode, DeserializationSigmaBuilder.Plus[SNumericType]),

//    ConstantSerializer(SByte),
//    ConstantSerializer(SInt),
//    ConstantSerializer(SBigInt),
//    ConstantSerializer(SBox),
//    ConstantSerializer(SAvlTree),
//    ConstantSerializer(SGroupElement),
    ConcreteCollectionSerializer,
    TupleSerializer,
    SelectFieldSerializer,
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
    UpcastSerializer,
  ).map(s => (s.opCode, s)).toMap

  private def serializable(v: Value[SType]): Value[SType] = v match {
    case upcast: Upcast[SType, _]@unchecked =>
      upcast.input
    case _ => v
  }

  def serialize(v: Value[SType]): Array[Byte] = serializable(v) match {
  // todo rename to table when all serializers are here
  val tableReaderSerializers: Map[OpCode, ValueSerializer[_ <: Value[SType]]] = Seq[ValueSerializer[_ <: Value[SType]]](
    TupleSerializer,
    SelectFieldSerializer,
    Relation3Serializer(IsMemberCode, IsMember.apply),
    QuadrupleSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](IfCode, If.apply),
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
    Relation2Serializer(GtCode, GT.apply[SType], Seq(Constraints.onlyInt2)),
    Relation2Serializer(GeCode, GE.apply[SType], Seq(Constraints.onlyInt2)),
    Relation2Serializer(LtCode, LT.apply[SType], Seq(Constraints.onlyInt2)),
    Relation2Serializer(LeCode, LE.apply[SType], Seq(Constraints.onlyInt2)),
    Relation2Serializer(EqCode, EQ.apply[SType], Seq(Constraints.sameType2)),
    Relation2Serializer(NeqCode, NEQ.apply[SType], Seq(Constraints.sameType2)),
    TwoArgumentsSerializer(XorCode, Xor.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply),
    TwoArgumentsSerializer(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializer(MinusCode, Minus[SNumericType]),
    TwoArgumentsSerializer(MultiplyCode, Multiply[SNumericType]),
    TwoArgumentsSerializer(DivisionCode, Divide[SNumericType]),
    TwoArgumentsSerializer(ModuloCode, Modulo[SNumericType]),
    TwoArgumentsSerializer(PlusCode, Plus[SNumericType]),
  ).map(s => (s.opCode, s)).toMap

  // todo should be used only as entry point (not by serializers)
  // todo make into wrapper for serialize(v,w)
  def serialize(v: Value[SType]): Array[Byte] = v match {
    case c: Constant[SType] =>
      val w = Serializer.startWriter()
      ConstantSerializer.serialize(c, w)
      w.toBytes
    case _ =>
      val opCode = v.opCode
      tableReaderSerializers.get(opCode) match {
        case Some(serFn: SigmaSerializer[Value[SType], v.type]@unchecked) =>
          val w = Serializer.startWriter()
          w.put(opCode)
          serFn.serializeBody(v, w)
          w.toBytes
        case None =>
          table.get(opCode) match {
            case Some(serFn: SigmaSerializer[Value[SType], v.type]@unchecked) =>
              opCode +: serFn.serializeBody(v)
            case None =>
              sys.error(s"Cannot find serializer for Value with opCode=$opCode: $v")
          }
      }
  }

  override def serialize(v: Value[SType], w: ByteWriter): Unit = v match {
    case c: Constant[SType] =>
      ConstantSerializer.serialize(c, w)
    case _ =>
      val opCode = v.opCode
      table.get(opCode) match {
        case Some(serFn: SigmaSerializer[Value[SType], v.type]@unchecked) =>
          w.put(opCode)
          serFn.serializeBody(v, w)
        case None =>
          sys.error(s"Cannot find serializer for Value with opCode=$opCode: $v")
      }
  }

  // todo remove when last call site is eliminated
  override def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed) = {
    val c = bytes(pos)
    if (c.toUByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      val r = Serializer.startReader(bytes, pos)
      val c = ConstantSerializer.deserialize(r)
      (c, r.consumed)
    }
    else {
      tableReaderSerializers.get(c).map { handler =>
        val r = Serializer.startReader(bytes, pos + 1)
        val v = handler.parseBody(r)
        (v, r.consumed + 1)
      }.getOrElse {
        val handler = table(c)
        val (v: Value[SType], consumed) = handler.parseBody(bytes, pos + 1)
        (v, consumed + 1)
      }
    }
  }

  override def deserialize(r: ByteReader): Value[SType] = {
    val firstByte = r.peekByte()
    if (firstByte.toUByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      ConstantSerializer.deserialize(r)
    }
    else {
      val opCode = r.getByte()
      table(opCode).parseBody(r)
    }
  }

  // todo switch to reader deserialize (make out of byte array) after all serializers are migrated
  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1
}
