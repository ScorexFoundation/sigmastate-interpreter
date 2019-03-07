package sigmastate.serialization.transformers

import sigmastate.SigmaTransformer
import sigmastate.Values.SigmaPropValue
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

case class SigmaTransformerSerializer[I <: SigmaPropValue, O <: SigmaPropValue]
(code: OpCode, cons: Seq[SigmaPropValue] => SigmaPropValue)
  extends ValueSerializer[SigmaTransformer[I, O]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: SigmaTransformer[I, O], w: SigmaByteWriter): Unit = {

    SerializeLog.logPrintf(true, true, false, "Items length")
    w.putUInt(obj.items.length)
    SerializeLog.logPrintf(false, true, false, "Items length")

    SerializeLog.logPrintf(true, true, false, "Items")
    obj.items.foreach(w.putValue)
    SerializeLog.logPrintf(false, true, false, "Items")
  }

  override def parseBody(r: SigmaByteReader): SigmaPropValue = {
    val itemsSize = r.getUInt().toInt
    val b = mutable.ArrayBuilder.make[SigmaPropValue]()
    for (_ <- 0 until itemsSize) {
      b += r.getValue().asInstanceOf[SigmaPropValue]
    }
    cons(b.result())
  }
}
