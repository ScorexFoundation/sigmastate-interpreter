package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, ConstantNode, ConstantPlaceholder}
import sigmastate.lang.SigmaBuilder

import scala.collection.mutable.ArrayBuffer

class ConstantStore(private val constants: IndexedSeq[Constant[SType]] = IndexedSeq()) {

  private val store: ArrayBuffer[Constant[SType]] = new ArrayBuffer[Constant[SType]]()
  store ++= constants

  def put[T <: SType](c: Constant[T])(implicit builder: SigmaBuilder): ConstantPlaceholder[T] = {
    store += c.asInstanceOf[Constant[SType]]
    val tpe = c.asInstanceOf[ConstantNode[T]].tpe
    builder.mkConstantPlaceholder[tpe.type](store.size - 1, tpe)
      .asInstanceOf[sigmastate.Values.ConstantPlaceholder[T]]
  }

  def get(index: Int): Constant[SType] = store(index)

  def getAll: IndexedSeq[Constant[SType]] = store.toIndexedSeq
}
