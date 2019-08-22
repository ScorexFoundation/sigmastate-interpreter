package sigmastate.serialization.transformers

import sigmastate.{SigmaTransformer, SigmaTransformerCompanion}
import sigmastate.Values.{ValueCompanion, SigmaPropValue}
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

case class SigmaTransformerSerializer[I <: SigmaPropValue, O <: SigmaPropValue]
(opDesc: SigmaTransformerCompanion, cons: Seq[SigmaPropValue] => SigmaPropValue)
  extends ValueSerializer[SigmaTransformer[I, O]] {

  override def serialize(obj: SigmaTransformer[I, O], w: SigmaByteWriter): Unit =
    w.putValues(obj.items, opDesc.argInfos(0))

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val itemsSize = r.getUInt().toInt
    val b = mutable.ArrayBuilder.make[SigmaPropValue]()
    for (_ <- 0 until itemsSize) {
      b += r.getValue().asInstanceOf[SigmaPropValue]
    }
    cons(b.result())
  }
}
