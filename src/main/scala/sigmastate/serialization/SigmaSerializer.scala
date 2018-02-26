package sigmastate.serialization

import sigmastate._

import scala.util.Try


trait Serializer[M] {
  def toBytes(obj: M): Array[Byte]

  def parseBytes(bytes: Array[Byte]): Try[M]
}


trait SigmaSerializer[V <: Value[_ <: SType]] extends Serializer[V] {
  import SigmaSerializer._

  val opCode: OpCode

  def parseBody: DeserializingFn

  def serializeBody: SerializingFn[V]

  override def toBytes(obj: V): Array[Byte] = serialize(obj)

  override def parseBytes(bytes: Array[Byte]): Try[V] = Try {
    deserialize(bytes).asInstanceOf[V]
  }
}

object SigmaSerializer extends App {
  type OpCode = Byte

  type Position = Int
  type Consumed = Int
  type DeserializingFn = (Array[Byte], Position) => (Value[_ <: SType], Consumed, SType.TypeCode)
  type SerializingFn[V <: Value[_ <: SType]] = V => Array[Byte]

  val IntConstantCode = 11: Byte
  val TaggedVariableCode = 1: Byte

  val LtCode = 21: Byte
  val LeCode = 22: Byte
  val GtCode = 23: Byte
  val GeCode = 24: Byte
  val EqCode = 25: Byte
  val NeqCode = 26: Byte


  val TrueCode = 12: Byte
  val FalseCode = 13: Byte

  val ConcreteCollectionCode = 35: Byte

  val serializers = Seq[SigmaSerializer[_ <: Value[_ <: SType]]](
    RelationSerializer(GtCode, GT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(GeCode, GE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LtCode, LT.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(LeCode, LE.apply, Seq(Constraints.onlyInt2)),
    RelationSerializer(EqCode, EQ.applyNonTyped, Seq(Constraints.sameType2)),
    RelationSerializer(NeqCode, NEQ.apply, Seq(Constraints.sameType2)),
    IntConstantSerializer,
    TrueLeafSerializer,
    FalseLeafSerializer,
    ConcreteCollectionSerializer,
    TaggedVariableSerializer
  )

  val table: Map[Value.PropositionCode, (DeserializingFn, SerializingFn[_ <: Value[_ <: SType]])] =
    serializers.map(s => s.opCode -> (s.parseBody, s.serializeBody)).toMap

  def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed, SType.TypeCode) = {
    val c = bytes(pos)
    val handler = table(c)
    val (v, consumed, tc) = handler._1(bytes, pos + 1)
    (v, consumed + 1, tc)
  }

  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1

  def serialize(v: Value[_ <: SType]) = {
    val opCode = v.opCode
    val serFn = table(opCode)._2.asInstanceOf[SerializingFn[v.type]]
    opCode +: serFn(v)
  }

  //todo: convert cases below into tests:

  println(deserialize(Array[Byte](21, 11, 0, 0, 0, 0, 0, 0, 0, 2,
    11, 0, 0, 0, 0, 0, 0, 0, 3)))

  val s: Value[SInt.type] = IntConstant(4)
  println(serialize(s))

  assert(deserialize(serialize(s)) == s)

  assert(Try(deserialize(Array[Byte](21, 12, 13))).isFailure, "LT(bool, bool) must fail")

  val gt = GT(IntConstant(6), IntConstant(5))
  println(deserialize(serialize(gt)))
  assert(deserialize(serialize(gt)) == gt)

  val eq = EQ(TrueLeaf, FalseLeaf)
  println(serialize(eq).mkString(","))
  assert(deserialize(serialize(eq)) == eq)

  //todo: make this expression does not compile?
  val eq2 = EQ(TrueLeaf, IntConstant(5))

  //concrete collection
  val cc = ConcreteCollection(IndexedSeq(IntConstant(5), IntConstant(6), IntConstant(7)))
  assert(deserialize(serialize(cc)) == cc)

  val tb = TaggedBox(21: Byte)
  assert(deserialize(serialize(tb)) == tb)

  val cc2 = ConcreteCollection(IndexedSeq(IntConstant(5), TaggedInt(21)))
  assert(deserialize(serialize(cc2)) == cc2)
}

object Constraints {
  type Constraint2 = (SType.TypeCode, SType.TypeCode) => Boolean
  type ConstraintN = Seq[SType.TypeCode] => Boolean

  def onlyInt2: Constraint2 = {case (tc1, tc2) => tc1 == SInt.typeCode && tc2 == SInt.typeCode}
  def sameType2: Constraint2 = {case (tc1, tc2) => tc1 == tc2}

  def sameTypeN: ConstraintN = {tcs => tcs.tail.forall(_ == tcs.head)}
}