package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate.lang.exceptions.SerializerException
import sigmastate.utils._

trait Serializer[TFamily, T <: TFamily] {

  def parseBody(r: ByteReader): TFamily
  def serializeBody(obj: T, w: ByteWriter): Unit
  def error(msg: String) = throw new SerializerException(msg, None)

  final def toBytes(obj: T): Array[Byte] = {
    val w = Serializer.startWriter()
    serializeBody(obj, w)
    w.toBytes
  }
}

object Serializer {
  type Position = Int
  type Consumed = Int

    /** Helper function to be use in serializers.
    * Starting position is marked and then used to compute number of consumed bytes.
    * val r = Serializer.startReader(bytes, pos)
    * val obj = r.getValue()
    * obj -> r.consumed */
  def startReader(bytes: Array[Byte], pos: Int = 0): ByteReader = {
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
}

trait SigmaSerializerCompanion[TFamily] {
  type Tag

  def getSerializer(opCode: Tag): SigmaSerializer[TFamily, _ <: TFamily]
  def deserialize(r: ByteReader): TFamily
  def serialize(v: TFamily, w: ByteWriter): Unit
}

