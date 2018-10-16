package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.Constant
import sigmastate.lang.SigmaBuilder
import sigmastate.utils.{ByteReader, ByteWriter}

import scala.collection.mutable.ArrayBuffer

class SerializedConstantPlaceholderStore(builder: SigmaBuilder) {

  private val store = ArrayBuffer[Constant[SType]]()
  def nonEmpty: Boolean = store.nonEmpty

  /**
    * Adds the constant and assign the placeholder index.
    *
    * Indices are not re-used for the same constant, since we want scripts to be equivalent if they
    * use different constant values. Every call with the same constant will return new index.
    * @param c constant
    * @return placeholder index
    */
  def put(c: Constant[SType]): Int = this.synchronized {
    store.append(c)
    store.length - 1
  }

  /**
    * Gets the deserialized constant by the index assigned on serialization.
    * @param placeholderIndex placeholder index of constant in the script (assigned via `put`)
    * @return constant
    */
  def get(placeholderIndex: Int): Constant[SType] =
    store(placeholderIndex)

  /**
    * Serializes the accumulated (via `put`) constants.
    * @param w writer
    */
  def serialize(w: ByteWriter): Unit = {
    val constantSerializer = ConstantSerializer(builder)
    if (store.isEmpty) return
    w.putUInt(store.size)
    store.foreach { c => constantSerializer.serialize(c, w) }
  }

  /**
    * Deserializes constants.
    * @param r reader
    * @return self (with deserialized constants)
    */
  def deserialize(r: ByteReader): SerializedConstantPlaceholderStore = {
    // todo Should we rather throw here? Or better, make it part of initialization.
    // There is no scenario of loading constants in addition to already stored.
    if (store.nonEmpty) store.clear()
    val constantsCount = r.getUInt().toInt
    val constantSerializer = ConstantSerializer(builder)
    for (_ <- 0 until constantsCount) {
      val c = constantSerializer.deserialize(r)
      store.append(c)
    }
    this
  }
}

