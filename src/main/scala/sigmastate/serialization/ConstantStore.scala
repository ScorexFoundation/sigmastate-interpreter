package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, ConstantNode, ConstantPlaceholder}
import sigmastate.lang.SigmaBuilder
import debox.Buffer

/** @hotspot used in deserialization (don't beautify this code) */
class ConstantStore(private val constants: IndexedSeq[Constant[SType]] = Array()) {

  private val store: Buffer[Constant[SType]] = Buffer.fromIterable(constants)

  def put[T <: SType](c: Constant[T])(implicit builder: SigmaBuilder): ConstantPlaceholder[T] = {
    store += c.asInstanceOf[Constant[SType]]
    val tpe = c.asInstanceOf[ConstantNode[T]].tpe
    builder.mkConstantPlaceholder[tpe.type](store.length - 1, tpe)
      .asInstanceOf[sigmastate.Values.ConstantPlaceholder[T]]
  }

  @inline final def get(index: Int): Constant[SType] = store(index)

  @inline final def getAll: IndexedSeq[Constant[SType]] = store.toArray()
}
