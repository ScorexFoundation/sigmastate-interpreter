package sigmastate.serialization.transformers

import sigmastate.SigmaTransformer
import sigmastate.Values.SigmaPropValue
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

case class SigmaTransformerSerializer[I <: SigmaPropValue, O <: SigmaPropValue]
(code: OpCode, cons: Seq[SigmaPropValue] => SigmaPropValue)
  extends ValueSerializer[SigmaTransformer[I, O]] {

  override val opCode: OpCode = code

  override def serialize(obj: SigmaTransformer[I, O], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.items.length)
    obj.items.foreach(w.putValue)
  }

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val itemsSize = r.getUInt().toInt
    val b = mutable.ArrayBuilder.make[SigmaPropValue]()
    for (_ <- 0 until itemsSize) {
      b += r.getValue().asInstanceOf[SigmaPropValue]
    }
    cons(b.result())
  }
}
