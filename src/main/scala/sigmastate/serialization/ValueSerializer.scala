package sigmastate.serialization

import sigmastate._
import Values._
import scala.util.Try


trait ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {
  import ValueSerializer._
  override val companion = ValueSerializer
  val opCode: OpCode

  override def toBytes(obj: V): Array[Byte] = serialize(obj)

  override def parseBytes(bytes: Array[Byte]): Try[V] = Try {
    deserialize(bytes).asInstanceOf[V]
  }
}

object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type OpCode = Byte
  type Tag = OpCode

  val TaggedVariableCode = 1: Byte

  val IntConstantCode = 11: Byte
  val TrueCode = 12: Byte
  val FalseCode = 13: Byte
  val UnitConstantCode = 14: Byte
  val ByteArrayConstantCode = 15: Byte

  val LtCode = 21: Byte
  val LeCode = 22: Byte
  val GtCode = 23: Byte
  val GeCode = 24: Byte
  val EqCode = 25: Byte
  val NeqCode = 26: Byte

  val ConcreteCollectionCode = 35: Byte
  val TupleCode = 36: Byte
  val AndCode = 37: Byte
  val OrCode = 38: Byte
  val SomeValueCode = 39: Byte
  val NoneValueCode = 40: Byte

  val table = Seq[ValueSerializer[_ <: Value[SType]]](
    RelationSerializer(GtCode, GT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(GeCode, GE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LtCode, LT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LeCode, LE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(EqCode, EQ.applyNonTyped, Seq(Constraints.sameType2)),
    RelationSerializer(NeqCode, NEQ.apply, Seq(Constraints.sameType2)),
    IntConstantSerializer,
    ByteArrayConstantSerializer,
    TrueLeafSerializer,
    FalseLeafSerializer,
    ConcreteCollectionSerializer,
    AndSerializer,
    OrSerializer,
    TaggedVariableSerializer
  ).map(s => (s.opCode, s)).toMap

  def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed) = {
    val c = bytes(pos)
    val handler = table(c)
    val (v, consumed) = handler.parseBody(bytes, pos + 1)
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