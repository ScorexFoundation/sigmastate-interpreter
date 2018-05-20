package sigmastate.serialization

import sigmastate._
import Values._

import scala.util.Try
import OpCodes._
import sigmastate.SCollection.SByteArray
import sigmastate.serialization.transformers._
import sigmastate.serialization.trees.{Relation3Serializer, Relation2Serializer, QuadrupelSerializer}
import sigmastate.utxo._
import Serializer.Consumed


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

object ValueSerializer
  extends SigmaSerializerCompanion[Value[SType]] {

  type Tag = OpCode

  val table: Map[OpCode, ValueSerializer[_ <: Value[SType]]] = Seq[ValueSerializer[_ <: Value[SType]]](

    Relation2Serializer(GtCode, GT.apply, Seq(Constraints.onlyInt2)),
    Relation2Serializer(GeCode, GE.apply, Seq(Constraints.onlyInt2)),
    Relation2Serializer(LtCode, LT.apply, Seq(Constraints.onlyInt2)),
    Relation2Serializer(LeCode, LE.apply, Seq(Constraints.onlyInt2)),
    Relation2Serializer(EqCode, EQ.applyNonTyped, Seq(Constraints.sameType2)),
    Relation2Serializer(NeqCode, NEQ.apply, Seq(Constraints.sameType2)),
    Relation3Serializer(IsMemberCode, IsMember.apply),
    QuadrupelSerializer[SBoolean.type, SInt.type, SInt.type, SInt.type](IfCode, If.apply),

    TwoArgumentsSerializer(XorCode, Xor.apply),
//    TwoArgumentsSerializer(AppendBytesCode, AppendBytes.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply),
    TwoArgumentsSerializer(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializer(MinusCode, Minus),
    TwoArgumentsSerializer(MultiplyCode, Multiply),
    TwoArgumentsSerializer(DivisionCode, Divide),
    TwoArgumentsSerializer(ModuloCode, Modulo),
    TwoArgumentsSerializer(PlusCode, Plus),

    ProveDiffieHellmanTupleSerializer,
    GroupElementSerializer,
    ProveDlogSerializer,
    ByteConstantSerializer,
    IntConstantSerializer,
    CaseObjectSerialization(TrueCode, TrueLeaf),
    CaseObjectSerialization(FalseCode, FalseLeaf),
    ConcreteCollectionSerializer,
    SimpleTransformerSerializer[SCollection[SBoolean.type], SBoolean.type](AndCode, AND.apply),
    SimpleTransformerSerializer[SCollection[SBoolean.type], SBoolean.type](OrCode, OR.apply),
    TaggedVariableSerializer,
    BigIntConstantSerializer,
    CollectionConstantSerializer,
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
    SimpleTransformerSerializer[SBox.type, SInt.type](ExtractAmountCode, ExtractAmount.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractScriptBytesCode, ExtractScriptBytes.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesCode, ExtractBytes.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesWithNoRefCode, ExtractBytesWithNoRef.apply),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractIdCode, ExtractId.apply),
    SimpleTransformerSerializer[SInt.type, SByteArray](IntToByteArrayCode, IntToByteArray.apply),
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
    BoxConstantSerializer,
    AvlTreeConstantSerializer
  ).map(s => (s.opCode, s)).toMap

  def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed) = {
    val c = bytes(pos)
    val handler = table(c)
    val (v: Value[SType], consumed) = handler.parseBody(bytes, pos + 1)
    (v, consumed + 1)
  }

  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1

  def serialize(v: Value[SType]): Array[Byte] = {
    val opCode = v.opCode
    val serFn = table(opCode).asInstanceOf[SigmaSerializer[Value[SType], v.type]]
    opCode +: serFn.serializeBody(v)
  }
}

object Constraints {
  type Constraint2 = (SType.TypeCode, SType.TypeCode) => Boolean
  type ConstraintN = Seq[SType.TypeCode] => Boolean

  def onlyInt2: Constraint2 = {
    case (tc1, tc2) => tc1 == SInt.typeCode && tc2 == SInt.typeCode
  }

  def sameType2: Constraint2 = {
    case (tc1, tc2) => tc1 == tc2
  }

  def sameTypeN: ConstraintN = { tcs => tcs.tail.forall(_ == tcs.head) }
}