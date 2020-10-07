package sigmastate.utils

import scorex.util.serialization.Reader
import sigmastate.SType
import sigmastate.Values.{SValue, Value}
import sigmastate.lang.exceptions.{InputSizeLimitExceeded, DeserializeCallDepthExceeded}
import sigmastate.serialization._
import scorex.util.Extensions._
import spire.syntax.all.cfor

class SigmaByteReader(val r: Reader,
                      var constantStore: ConstantStore,
                      var resolvePlaceholdersToConstants: Boolean,
                      val maxTreeDepth: Int = SigmaSerializer.MaxTreeDepth)
  extends Reader {

  @inline private def checkPositionLimit(): Unit =
    if (position > positionLimit)
      throw new InputSizeLimitExceeded(s"SigmaByteReader position limit $positionLimit is reached at position $position")

  /** The reader should be lightweight to create. In most cases ErgoTrees don't have
    * ValDef nodes hence the store is not necessary and it's initialization dominates the
    * reader instantiation time. Hence it's lazy.
    * @hotspot
    */
  lazy val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()

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

  @inline def getType(): SType = TypeSerializer.deserialize(this)
  @inline def getValue(): SValue = ValueSerializer.deserialize(this)

  private var lvl: Int = 0
  @inline def level: Int = lvl
  @inline def level_=(v: Int): Unit = {
    if (v > maxTreeDepth)
      throw new DeserializeCallDepthExceeded(s"nested value deserialization call depth($v) exceeds allowed maximum $maxTreeDepth")
    lvl = v
  }

  /** Read sequence of values from this reader.
    * It first reads the number of values and then reads each value using `getValue` method.
    *
    * @return a sequence of zero of more values read
    */
  @inline def getValues(): IndexedSeq[SValue] = {
    val size = getUInt().toIntExact
    if (size == 0) Value.EmptySeq // quick short-cut when there is nothing to read
    else {
      val xs = new Array[SValue](size)
      cfor(0)(_ < size, _ + 1) { i =>
        xs(i) = getValue()
      }
      xs
    }
  }

  private var positionLmt: Int = r.position + r.remaining
  @inline final def positionLimit: Int = positionLmt
  @inline final def positionLimit_=(v: Int): Unit = {
    positionLmt = v
  }

  private var _complexity: Int = 0
  @inline final def complexity: Int = _complexity
  @inline final def complexity_=(v: Int): Unit = {
    _complexity = v
  }

  @inline final def addComplexity(delta: Int): Unit = {
    _complexity += delta
  }
}
