package sigma.serialization

import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{Serializer, VLQByteBufferReader, VLQByteBufferWriter}
import sigma.data.SigmaConstants

import java.nio.ByteBuffer

abstract class CoreSerializer[TFamily, T <: TFamily] extends Serializer[TFamily, T, CoreByteReader, CoreByteWriter] {

  def error(msg: String) = throw new SerializerException(msg, None)

  final def toBytes(obj: T): Array[Byte] = {
    val w = CoreSerializer.startWriter()
    serialize(obj, w)
    w.toBytes
  }

  final def fromBytes(bytes: Array[Byte]): TFamily = {
    parse(CoreSerializer.startReader(bytes))
  }
}

object CoreSerializer {
  type Position = Int

  val MaxPropositionSize: Int = SigmaConstants.MaxPropositionBytes.value
  val MaxTreeDepth: Int = SigmaConstants.MaxTreeDepth.value

  /** Helper function to be use in serializers.
    * Starting position is marked and then used to compute number of consumed bytes.
    * val r = Serializer.startReader(bytes, pos)
    * val obj = r.getValue()
    * obj -> r.consumed
    */
  def startReader(bytes: Array[Byte],
      pos: Int = 0): CoreByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(pos)
    val r = new CoreByteReader(
      new VLQByteBufferReader(buf),
      maxTreeDepth = MaxTreeDepth
    ).mark()
    r
  }

  /** Helper function to be use in serializers. */
  def startReader(bytes: Array[Byte]): CoreByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    val r = new CoreByteReader(new VLQByteBufferReader(buf),
      maxTreeDepth = MaxTreeDepth).mark()
    r
  }

  /** Helper function to be use in serializers.
    * val w = Serializer.startWriter()
    * w.putLong(l)
    * val res = w.toBytes
    * res */
  def startWriter(): CoreByteWriter = {
    val b = new ByteArrayBuilder()
    val wi = new VLQByteBufferWriter(b)
    val w = new CoreByteWriter(wi)
    w
  }

}

