package sigmastate.serialization

import java.nio.ByteBuffer

import scala.util.Try
import Serializer.{Position, Consumed}
import sigmastate.utils._

trait Serializer[TFamily, T <: TFamily] {
  def toBytes(obj: T): Array[Byte]

  def parseBytes(bytes: Array[Byte]): Try[TFamily] = Try {
    parseBody(bytes, 0)._1
  }

  def parseBody(bytes: Array[Byte], pos: Position): (TFamily, Consumed)
  def serializeBody(obj: T): Array[Byte] = toBytes(obj)
}

object Serializer {
  type Position = Int
  type Consumed = Int

    /** Helper function to be use in serializers.
    * Starting position is marked and then used to compute number of consumed bytes.
    * val r = Serializer.startReader(bytes, pos)
    * val obj = r.getValue()
    * obj -> r.consumed */
  def startReader(bytes: Array[Byte], pos: Int): ByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(pos)
    val r = new ByteBufferReader(buf)
        .mark()
    r
  }

  /** Helper function to be use in serializers.
    * val w = Serializer.startWriter()
    * w.putLong(l)
    * val res = w.toBytes
    * res */
  def startWriter(): ByteWriter = {
    val b = new ByteArrayBuilder()
    val w = new ByteArrayWriter(b)
    w
  }
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

