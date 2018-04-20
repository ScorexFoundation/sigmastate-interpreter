package sigmastate.serialization

import sigmastate._
import Values._
import scala.util.Try
import OpCodes._


trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {
  import ValueSerializer._
  override val companion = ValueSerializer
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

    RelationSerializer(GtCode, GT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(GeCode, GE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LtCode, LT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LeCode, LE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(EqCode, EQ.applyNonTyped, Seq(Constraints.sameType2)),
    RelationSerializer(NeqCode, NEQ.apply, Seq(Constraints.sameType2)),

    TwoArgumentsSerializer(XorCode, Xor.apply),
    TwoArgumentsSerializer(AppendBytesCode, AppendBytes.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply),
    TwoArgumentsSerializer(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializer(MinusCode, Minus.apply),
    TwoArgumentsSerializer(PlusCode, Plus.apply),

    GroupElementSerializer,
    IntConstantSerializer,
    TrueLeafSerializer,
    FalseLeafSerializer,
    ConcreteCollectionSerializer,
    AndSerializer,
    OrSerializer,
    TaggedVariableSerializer,
    NotSerializer,
    BigIntConstantSerializer,
    ByteArrayConstantSerializer
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

  def onlyInt2: Constraint2 = {case (tc1, tc2) => tc1 == SInt.typeCode && tc2 == SInt.typeCode}
  def sameType2: Constraint2 = {case (tc1, tc2) => tc1 == tc2}

  def sameTypeN: ConstraintN = {tcs => tcs.tail.forall(_ == tcs.head)}
}