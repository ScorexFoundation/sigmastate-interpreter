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

trait OpCodes {

  type OpCode = Byte

  val TaggedVariableCode:   OpCode = 1: Byte

  // EvaluatedValue descendants

  val IntConstantCode:          OpCode = 11: Byte
  val TrueCode:                 OpCode = 12: Byte
  val FalseCode:                OpCode = 13: Byte
  val UnitConstantCode:         OpCode = 14: Byte
  val BigIntConstantCode:       OpCode = 15: Byte
  val ByteArrayConstantCode:    OpCode = 16: Byte
  val GroupElementConstantCode: OpCode = 17: Byte

  // Relation descendants

  val LtCode:     OpCode = 21: Byte
  val LeCode:     OpCode = 22: Byte
  val GtCode:     OpCode = 23: Byte
  val GeCode:     OpCode = 24: Byte
  val EqCode:     OpCode = 25: Byte
  val NeqCode:    OpCode = 26: Byte

  val ConcreteCollectionCode: OpCode = 35: Byte

  val TupleCode:  OpCode = 36: Byte
  val AndCode:    OpCode = 37: Byte
  val OrCode:     OpCode = 38: Byte
  val NotCode:    OpCode = 39: Byte

  // TwoArgumentsOperation descendants

  val MinusCode:          OpCode = 40: Byte
  val PlusCode:           OpCode = 41: Byte
  val XorCode:            OpCode = 42: Byte
  val AppendBytesCode:    OpCode = 43: Byte
  val ExponentiateCode:   OpCode = 44: Byte
  val MultiplyGroupCode:  OpCode = 45: Byte
}

object ValueSerializer
  extends SigmaSerializerCompanion[Value[SType]]
  with OpCodes {

  override type OpCode = Byte
  type Tag = OpCode

  val table = Seq[ValueSerializer[_ <: Value[SType]]](

    RelationSerializer(GtCode, GT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(GeCode, GE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LtCode, LT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LeCode, LE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(EqCode, EQ.applyNonTyped, Seq(Constraints.sameType2)),
    RelationSerializer(NeqCode, NEQ.apply, Seq(Constraints.sameType2)),

    TwoArgumentsSerializerChecked(XorCode, Xor.apply),
    TwoArgumentsSerializerChecked(AppendBytesCode, AppendBytes.apply),
    TwoArgumentsSerializer(ExponentiateCode, Exponentiate.apply, Seq(Constraints.sameType2)),
    TwoArgumentsSerializerChecked(MultiplyGroupCode, MultiplyGroup.apply),
    TwoArgumentsSerializerChecked(MinusCode, Minus.apply),
    TwoArgumentsSerializerChecked(PlusCode, Plus.apply),

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