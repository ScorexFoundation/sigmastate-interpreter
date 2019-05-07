package sigmastate.utils

import scorex.util.Extensions._
import scorex.util.serialization.Reader
import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.lang.exceptions.DeserializeCallDepthExceeded
import sigmastate.serialization.{ConstantStore, TypeSerializer, ValDefTypeStore, ValueSerializer}

class SigmaByteReader(val r: Reader,
                      var constantStore: ConstantStore,
                      var resolvePlaceholdersToConstants: Boolean,
                      val maxTreeDepth: Int)
  extends Reader {

  val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()


  override type CH = r.CH

  @inline
  override def newReader(chunk: CH): Reader.Aux[CH] = r.newReader(chunk)

  @inline override def getChunk(size: Int): CH = r.getChunk(size)

  @inline override def getShortString(): String = r.getShortString()

  @inline override def peekByte(): Byte = r.peekByte()

  @inline override def getByte(): Byte = r.getByte()

  @inline override def getUByte(): Int = r.getUByte()

  @inline override def getShort(): Short = r.getShort()

  @inline override def getUShort(): Int = r.getUShort()

  @inline override def getInt(): Int = r.getInt()

  @inline override def getUInt(): Long = r.getUInt()

  @inline override def getLong(): Long = r.getLong()

  @inline override def getULong(): Long = r.getULong()

  @inline override def getBytes(size: Int): Array[Byte] = r.getBytes(size)

  @inline override def getBits(size: Int): Array[Boolean] = r.getBits(size)

  @inline override def getOption[T](getValue: => T): Option[T] = r.getOption(getValue)

  @inline override def consumed: Int = r.consumed

  @inline override def position: Int = r.position

  @inline override def position_=(p: Int): Unit = r.position_=(p)

  @inline override def remaining: Int = r.remaining

  @inline override def mark(): this.type = {
    r.mark()
    this
  }

  @inline def getType(): SType = TypeSerializer.deserialize(this)
  @inline def getValue(): SValue = ValueSerializer.deserialize(this)

  private var lvl: Int = 0
  @inline def level: Int = lvl
  @inline def level_=(v: Int): Unit = {
    if (v > maxTreeDepth)
      throw new DeserializeCallDepthExceeded(s"nested value deserialization call depth($v) exceeds allowed maximum $maxTreeDepth")
    lvl = v
  }

  @inline def getValues(): IndexedSeq[SValue] = {
    val size = getUInt().toIntExact
    val xs = new Array[SValue](size)
    for (i <- 0 until size) {
      xs(i) = getValue()
    }
    xs.toIndexedSeq
  }
}
