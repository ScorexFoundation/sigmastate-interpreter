package sigmastate.serialization

import scala.util.Try
import Serializer.{Position, Consumed}

trait Serializer[TFamily, T <: TFamily] {
  def toBytes(obj: T): Array[Byte]

  def parseBytes(bytes: Array[Byte]): Try[TFamily]

  def parseBody(bytes: Array[Byte], pos: Position): (TFamily, Consumed)
  def serializeBody(obj: T): Array[Byte] = toBytes(obj)
}

object Serializer {
  type Position = Int
  type Consumed = Int
}

trait SigmaSerializer[TFamily, T <: TFamily] extends Serializer[TFamily, T] {
  val companion: SigmaSerializerCompanion[TFamily]

  def parseBody(bytes: Array[Byte], pos: Position): (TFamily, Consumed)
  def serializeBody(obj: T): Array[Byte]
}

trait SigmaSerializerCompanion[TFamily] {
  type Tag
  val table: Map[Tag, SigmaSerializer[TFamily, _]]
  def deserialize(bytes: Array[Byte], pos: Position): (TFamily, Consumed)
  def serialize(v: TFamily): Array[Byte]
}

