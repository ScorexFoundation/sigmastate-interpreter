package sigmastate.utils

import scorex.util.serialization.Reader
import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.lang.exceptions.{DeserializeCallDepthExceeded, InputSizeLimitExceeded}
import sigmastate.serialization._
import scorex.util.Extensions._

class SigmaByteReader(val r: Reader,
                      var constantStore: ConstantStore,
                      var resolvePlaceholdersToConstants: Boolean,
                      val maxTreeDepth: Int = SigmaSerializer.MaxTreeDepth)
  extends Reader {

  private def checkPositionLimit(): Unit =
    if (position > positionLimit)
      throw new InputSizeLimitExceeded(s"read bytes position limit $positionLimit is reached at position $position")


  val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()


  override type CH = r.CH

  @inline
  override def newReader(chunk: CH): Reader.Aux[CH] = r.newReader(chunk)

  @inline override def getChunk(size: Int): CH = r.getChunk(size)

  @inline override def getShortString(): String = {
    checkPositionLimit()
    r.getShortString()
  }

  @inline override def peekByte(): Byte = r.peekByte()

  @inline override def getByte(): Byte = {
    checkPositionLimit()
    r.getByte()
  }

  @inline override def getUByte(): Int = {
    checkPositionLimit()
    r.getUByte()
  }

  @inline override def getShort(): Short = {
    checkPositionLimit()
    r.getShort()
  }

  @inline override def getUShort(): Int = {
    checkPositionLimit()
    r.getUShort()
  }

  @inline override def getInt(): Int = {
    checkPositionLimit()
    r.getInt()
  }

  @inline override def getUInt(): Long = {
    checkPositionLimit()
    r.getUInt()
  }

  @inline override def getLong(): Long = {
    checkPositionLimit()
    r.getLong()
  }

  @inline override def getULong(): Long = {
    checkPositionLimit()
    r.getULong()
  }

  @inline override def getBytes(size: Int): Array[Byte] = {
    checkPositionLimit()
    r.getBytes(size)
  }

  @inline override def getBits(size: Int): Array[Boolean] = {
    checkPositionLimit()
    r.getBits(size)
  }

  @inline override def getOption[T](getValue: => T): Option[T] = {
    checkPositionLimit()
    r.getOption(getValue)
  }

  @inline override def consumed: Int = r.consumed

  @inline override def position: Int = r.position

  @inline override def position_=(p: Int): Unit = r.position_=(p)

  @inline override def remaining: Int = r.remaining

  @inline override def mark(): this.type = {
    r.mark()
    this
  }

  @inline def getType(): SType = {
    checkPositionLimit()
    TypeSerializer.deserialize(this)
  }
  @inline def getValue(): SValue = {
    checkPositionLimit()
    ValueSerializer.deserialize(this)
  }

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

  private var positionLmt: Int = r.position + r.remaining
  @inline def positionLimit: Int = positionLmt
  @inline def positionLimit_=(v: Int): Unit = {
    positionLmt = v
  }
}
