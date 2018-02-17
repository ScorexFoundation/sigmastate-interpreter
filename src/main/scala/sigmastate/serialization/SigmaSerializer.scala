package sigmastate.serialization

import scorex.core.serialization.Serializer
import sigmastate._

import scala.util.Try

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
  type DeserializingFn = (Array[Byte], Position) => (Value[_ <: SType], Consumed)
  type SerializingFn[V <: Value[_ <: SType]] = V => Array[Byte]

  val IntConstantCode = 11: Byte
  val GeCode = 21: Byte
  val TrueCode = 12: Byte
  val FalseCode = 13: Byte

  val ConcreteCollectionCode = 25: Byte
  val ConcreteCollectionDeserializer: DeserializingFn = {
    case (bytes, pos) => ???
  }
  val ConcreteCollectionSerializer: SerializingFn[ConcreteCollection[_]] = {cc => ???}

  val serializers = Seq[SigmaSerializer[_ <: Value[_ <: SType]]](
    RelationSerializer[GE](GeCode),
    IntConstantSerializer,
    TrueLeafSerializer,
    FalseLeafSerializer
  )

  val table: Map[Value.PropositionCode, (DeserializingFn, SerializingFn[_ <: Value[_ <: SType]])] =
    serializers.map(s => s.opCode -> (s.parseBody, s.serializeBody)).toMap

  def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed) = {
    val c = bytes(pos)
    val (v, consumed) = table(c)._1(bytes, pos + 1)
    v -> (consumed + 1)
  }

  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1

  def serialize(v: Value[_ <: SType]) = {
    val opCode = v.opCode
    val serFn = table(opCode)._2.asInstanceOf[SerializingFn[v.type]]
    opCode +: serFn(v)
  }

  println(deserialize(Array[Byte](21, 11, 0, 0, 0, 0, 0, 0, 0, 2,
    11, 0, 0, 0, 0, 0, 0, 0, 3)))


  println(deserialize(Array[Byte](21, 12, 13)))


  val s: Value[SInt.type] = IntConstant(4)
  println(serialize(s))

  assert(deserialize(serialize(s)) == s)
}