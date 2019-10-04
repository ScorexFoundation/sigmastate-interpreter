package sigmastate.serialization.transformers

import scalan.util.Extensions.LongOps
import sigmastate.{SigmaTransformer, SigmaTransformerCompanion}
import sigmastate.Values.{ValueCompanion, SigmaPropValue}
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.SigmaByteWriter.{DataInfo, valuesItemInfo}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import spire.syntax.all.cfor

case class SigmaTransformerSerializer[I <: SigmaPropValue, O <: SigmaPropValue]
(opDesc: SigmaTransformerCompanion, cons: Seq[SigmaPropValue] => SigmaPropValue)
  extends ValueSerializer[SigmaTransformer[I, O]] {
  val itemsInfo: DataInfo[Seq[SValue]] = opDesc.argInfos(0)
  val itemsItemInfo = valuesItemInfo(itemsInfo)

  override def serialize(obj: SigmaTransformer[I, O], w: SigmaByteWriter): Unit =
    w.putValues(obj.items, itemsInfo, itemsItemInfo)

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val itemsSize = r.getUInt().toIntExact    // HF change: was r.getUInt().toInt
    val res = new Array[SigmaPropValue](itemsSize)
    cfor(0)(_ < itemsSize, _ + 1) { i =>
      res(i) = r.getValue().asInstanceOf[SigmaPropValue]
    }
    cons(res)
  }
}
