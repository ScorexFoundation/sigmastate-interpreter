package sigmastate.serialization

import sigmastate._
import Values._

import scala.util.Try
import OpCodes._
import sigmastate.SCollection.SByteArray
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{QuadrupelSerializer, Relation2Serializer, Relation3Serializer}
import sigmastate.utxo._
import sigmastate.utils.Extensions._
import Serializer.Consumed
import org.ergoplatform._
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.Constraints.{onlyNumeric2, sameType2}


trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  import ValueSerializer._

  override val companion = ValueSerializer

  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  val opCode: OpCode

  override def toBytes(obj: V): Array[Byte] = serialize(obj)

  override def parseBytes(bytes: Array[Byte]): Try[V] = Try {
    deserialize(bytes).asInstanceOf[V]
  }
}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  val table: Map[OpCode, ValueSerializer[_ <: Value[SType]]] = Seq[ValueSerializer[_ <: Value[SType]]](

    Relation2Serializer(GtCode, GT.apply[SType], Seq(onlyNumeric2, sameType2)),
    Relation2Serializer(GeCode, GE.apply[SType], Seq(onlyNumeric2, sameType2)),
    Relation2Serializer(LtCode, LT.apply[SType], Seq(onlyNumeric2, sameType2)),
    Relation2Serializer(LeCode, LE.apply[SType], Seq(onlyNumeric2, sameType2)),
    Relation2Serializer(EqCode, DeserializationSigmaBuilder.EQ[SType], Seq()),
    Relation2Serializer(NeqCode, NEQ.apply[SType], Seq(sameType2)),
    Relation3Serializer(IsMemberCode, IsMember.apply),
    QuadrupelSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](IfCode, If.apply),

    TwoArgumentsSerializer(XorCode, Xor.apply),
//    TwoArgumentsSerializer(AppendBytesCode, AppendBytes.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply),
    TwoArgumentsSerializer(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializer(MinusCode, Minus[SNumericType]),
    TwoArgumentsSerializer(MultiplyCode, Multiply[SNumericType]),
    TwoArgumentsSerializer(DivisionCode, Divide[SNumericType]),
    TwoArgumentsSerializer(ModuloCode, Modulo[SNumericType]),
    TwoArgumentsSerializer(PlusCode, Plus[SNumericType]),

    ProveDiffieHellmanTupleSerializer,
    ProveDlogSerializer,
//    ConstantSerializer(SByte),
//    ConstantSerializer(SInt),
//    ConstantSerializer(SBigInt),
//    ConstantSerializer(SBox),
//    ConstantSerializer(SAvlTree),
//    ConstantSerializer(SGroupElement),
    CaseObjectSerialization(SBoolean.typeCode, TrueLeaf),
    CaseObjectSerialization(SBoolean.typeCode, FalseLeaf),
    ConcreteCollectionSerializer,
    TupleSerializer,
    SelectFieldSerializer,
    ConcreteCollectionBooleanConstantSerializer,
    LogicalTransformerSerializer(AndCode, AND.apply),
    LogicalTransformerSerializer(OrCode, OR.apply),
    TaggedVariableSerializer,
    CaseObjectSerialization(HeightCode, Height),
    CaseObjectSerialization(InputsCode, Inputs),
    CaseObjectSerialization(OutputsCode, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHashCode, LastBlockUtxoRootHash),
    CaseObjectSerialization(SelfCode, Self),
    CaseObjectSerialization(GroupGeneratorCode, GroupGenerator),
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
    AppendSerializer
  ).map(s => (s.opCode, s)).toMap

  private def serializable(v: Value[SType]): Value[SType] = v match {
    case upcast: Upcast[SType, _]@unchecked =>
      upcast.input
    case _ => v
  }

  def serialize(v: Value[SType]): Array[Byte] = serializable(v) match {
    case c: Constant[SType] =>
      val w = Serializer.startWriter()
      ConstantSerializer.serialize(c, w)
      w.toBytes
    case _ =>
      val opCode = v.opCode
      table.get(opCode) match {
        case Some(serFn: SigmaSerializer[Value[SType], v.type]@unchecked) =>
          opCode +: serFn.serializeBody(v)
        case None =>
          sys.error(s"Cannot find serializer for Value with opCode=$opCode: $v")
      }
  }

  def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed) = {
    val c = bytes(pos)
    if (c.toUByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      val r = Serializer.startReader(bytes, pos)
      val c = ConstantSerializer.deserialize(r)
      (c, r.consumed)
    }
    else {
      val handler = table(c)
      val (v: Value[SType], consumed) = handler.parseBody(bytes, pos + 1)
      (v, consumed + 1)
    }
  }

  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1
}

object Constraints {
  type Constraint2 = (SType.TypeCode, SType.TypeCode) => Boolean
  type TypeConstraint2 = (SType, SType) => Boolean
  type ConstraintN = Seq[SType.TypeCode] => Boolean

  def onlyNumeric2: TypeConstraint2 = {
    case (_: SNumericType, _: SNumericType) => true
    case _ => false
  }

  def sameType2: TypeConstraint2 = {
    case (v1, v2) => v1.tpe == v2.tpe
  }

  def sameTypeN: ConstraintN = { tcs => tcs.tail.forall(_ == tcs.head) }
}
