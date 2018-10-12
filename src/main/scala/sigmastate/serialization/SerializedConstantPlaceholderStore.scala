package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.Constant
import sigmastate.lang.SigmaBuilder
import sigmastate.utils.{ByteReader, ByteWriter}

import scala.collection.mutable

class SerializedConstantPlaceholderStore(builder: SigmaBuilder) {

  private val store = mutable.Map[Constant[SType], Int]()
  private val reversedStore = mutable.Map[Int, Constant[SType]]()
  private var lastFreeIndex = 0

  def nonEmpty: Boolean = store.nonEmpty

  def getIndex(c: Constant[SType]): Int = {
    store.get(c) match {
      case Some(v) => v
      case None =>
        val usedIndex = lastFreeIndex
        store.put(c, usedIndex)
        reversedStore.put(usedIndex, c)
        lastFreeIndex = lastFreeIndex + 1
        usedIndex
    }
  }

  def getConstant(placeholderIndex: Int): Constant[SType] =
    reversedStore(placeholderIndex)

  def serialize(w: ByteWriter): Unit = {
    val constantSerializer = ConstantSerializer(builder)
    if (store.isEmpty) return
    w.putUInt(store.size)
    store.foreach { case (c, index) =>
      constantSerializer.serialize(c, w)
      w.putUInt(index)
    }
  }

  def deserialize(r: ByteReader): SerializedConstantPlaceholderStore = {
    if (store.nonEmpty) store.clear()
    if (reversedStore.nonEmpty) reversedStore.clear()
    val constantsCount = r.getUInt().toInt
    val constantSerializer = ConstantSerializer(builder)
    for (_ <- 0 until constantsCount) {
      val c = constantSerializer.deserialize(r)
      val index = r.getUInt().toInt
      store.put(c, index)
      reversedStore.put(index, c)
    }
    this
  }
}

