package sigmastate.serialization

import java.nio.ByteBuffer

import sigma.util.ByteArrayBuilder
import sigmastate.lang.exceptions.SerializerException
import sigmastate.utils._

trait Serializer[TFamily, T <: TFamily] {

  def parseBody(r: SigmaByteReader): TFamily
  def serializeBody(obj: T, w: SigmaByteWriter): Unit
  def error(msg: String) = throw new SerializerException(msg, None)

  final def toBytes(obj: T): Array[Byte] = {
    val w = Serializer.startWriter()
    serializeBody(obj, w)
    w.toBytes
  }

  final def fromBytes(bytes: Array[Byte]): TFamily = {
    val r = Serializer.startReader(bytes)
    parseBody(r)
  }
}

object Serializer {
  type Position = Int
  type Consumed = Int

  val MaxInputSize: Int = 1024 * 1024 * 1
  val MaxTreeDepth: Int = 100

    /** Helper function to be use in serializers.
    * Starting position is marked and then used to compute number of consumed bytes.
    * val r = Serializer.startReader(bytes, pos)
    * val obj = r.getValue()
    * obj -> r.consumed */
  def startReader(bytes: Array[Byte], pos: Int = 0): SigmaByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(pos)
    val r = new SigmaByteReader(buf, new ConstantStore(), resolvePlaceholdersToConstants = false)
        .mark()
    r
  }

  def startReader(bytes: Array[Byte],
                  constantStore: ConstantStore,
                  resolvePlaceholdersToConstants: Boolean): SigmaByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    val r = new SigmaByteReader(buf, constantStore, resolvePlaceholdersToConstants)
      .mark()
    r
  }

  /** Helper function to be use in serializers.
    * val w = Serializer.startWriter()
    * w.putLong(l)
    * val res = w.toBytes
    * res */
  def startWriter(): SigmaByteWriter = {
    val b = new ByteArrayBuilder()
    val w = new SigmaByteWriter(b, constantExtractionStore = None)
    w
  }

  def startWriter(constantExtractionStore: ConstantStore): SigmaByteWriter = {
    val b = new ByteArrayBuilder()
    val w = new SigmaByteWriter(b, constantExtractionStore = Some(constantExtractionStore))
    w
  }
}

trait SigmaSerializer[TFamily, T <: TFamily] extends Serializer[TFamily, T] {
  val companion: SigmaSerializerCompanion[TFamily]
}

trait SigmaSerializerCompanion[TFamily] {
  type Tag

  def getSerializer(opCode: Tag): SigmaSerializer[TFamily, _ <: TFamily]
  def deserialize(r: SigmaByteReader): TFamily
  def serialize(v: TFamily, w: SigmaByteWriter): Unit
}

