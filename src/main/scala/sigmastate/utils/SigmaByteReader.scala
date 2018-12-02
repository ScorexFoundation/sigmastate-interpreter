package sigmastate.utils

import java.nio.ByteBuffer

import scorex.util.serialization.VLQByteBufferReader
import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.serialization.{ValDefTypeStore, TypeSerializer, ValueSerializer, ConstantStore}
import scorex.util.Extensions._

class SigmaByteReader(b: ByteBuffer,
                      var constantStore: ConstantStore,
                      var resolvePlaceholdersToConstants: Boolean)
  extends VLQByteBufferReader(b) {

  val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()

  @inline override def mark(): this.type = {
    super.mark()
    this
  }

  @inline def getType(): SType = TypeSerializer.deserialize(this)
  @inline def getValue(): SValue = ValueSerializer.deserialize(this)

  private var lvl: Int = 0
  @inline def level: Int = lvl
  @inline def level_=(v: Int): Unit = lvl = v
  @inline def getValues(): IndexedSeq[SValue] = {
    val size = getUInt().toIntExact
    val xs = new Array[SValue](size)
    for (i <- 0 until size) {
      xs(i) = getValue()
    }
    xs.toIndexedSeq
  }
}
