package sigmastate.serialization

import java.nio.ByteBuffer

import org.ergoplatform.SigmaConstants
import org.ergoplatform.validation.SigmaValidationSettings
import scorex.util.ByteArrayBuilder
import sigmastate.lang.exceptions.SerializerException
import sigmastate.utils._
import scorex.util.serialization._
import sigmastate.serialization.OpCodes.OpCode

object SigmaSerializer {
  type Position = Int
  type Consumed = Int

  val MaxPropositionSize: Int = SigmaConstants.MaxPropositionBytes.value
  val MaxTreeDepth: Int = SigmaConstants.MaxTreeDepth.value

    /** Helper function to be use in serializers.
    * Starting position is marked and then used to compute number of consumed bytes.
    * val r = Serializer.startReader(bytes, pos)
    * val obj = r.getValue()
    * obj -> r.consumed */
  def startReader(bytes: Array[Byte], pos: Int = 0): SigmaByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(pos)
    val r = new SigmaByteReader(new VLQByteBufferReader(buf),
      new ConstantStore(),
      resolvePlaceholdersToConstants = false,
      maxTreeDepth = MaxTreeDepth).mark()
    r
  }

  def startReader(bytes: Array[Byte],
                  constantStore: ConstantStore,
                  resolvePlaceholdersToConstants: Boolean)(implicit vs: SigmaValidationSettings): SigmaByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    val r = new SigmaByteReader(new VLQByteBufferReader(buf),
      constantStore,
      resolvePlaceholdersToConstants,
      maxTreeDepth = MaxTreeDepth).mark()
    r
  }

  /** Helper function to be use in serializers.
    * val w = Serializer.startWriter()
    * w.putLong(l)
    * val res = w.toBytes
    * res */
  def startWriter(): SigmaByteWriter = {
    val b = new ByteArrayBuilder()
    val wi = new VLQByteBufferWriter(b)
    val w = new SigmaByteWriter(wi, constantExtractionStore = None)
    w
  }

  def startWriter(constantExtractionStore: ConstantStore): SigmaByteWriter = {
    val b = new ByteArrayBuilder()
    val wi = new VLQByteBufferWriter(b)
    val w = new SigmaByteWriter(wi, constantExtractionStore = Some(constantExtractionStore))
    w
  }
}

trait SigmaSerializer[TFamily, T <: TFamily] extends Serializer[TFamily, T, SigmaByteReader, SigmaByteWriter] {

  def serializeWithGenericWriter(obj: T, w: Writer): Unit = {
    serialize(obj, new SigmaByteWriter(w, None))
  }

  def parseWithGenericReader(r: Reader)(implicit vs: SigmaValidationSettings): TFamily = {
    val sigmaByteReader = new SigmaByteReader(r,
      new ConstantStore(),
      resolvePlaceholdersToConstants = false,
      maxTreeDepth = SigmaSerializer.MaxTreeDepth)
    parse(sigmaByteReader)
  }

  def error(msg: String) = throw new SerializerException(msg, None)

  final def toBytes(obj: T): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serialize(obj, w)
    w.toBytes
  }

  final def fromBytes(bytes: Array[Byte]): TFamily = {
    parse(SigmaSerializer.startReader(bytes))
  }
}

trait SigmaSerializerCompanion[TFamily] {

  def getSerializer(opCode: OpCode): SigmaSerializer[TFamily, _ <: TFamily]
  def deserialize(r: SigmaByteReader): TFamily
  def serialize(v: TFamily, w: SigmaByteWriter): Unit
}

