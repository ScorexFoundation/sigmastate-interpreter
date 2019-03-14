package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class BlockValueSerializer(cons: (IndexedSeq[BlockItem], Value[SType]) => Value[SType])
  extends ValueSerializer[BlockValue] {

  override val opCode: OpCode = BlockValueCode

  override def serializeBody(obj: BlockValue, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "BlockValue")

    SerializeLog.logPrintf(true, true, false, "items.length")
    w.putUInt(obj.items.length)
    SerializeLog.logPrintf(false, true, false, "items.length")

    SerializeLog.logPrintf(true, true, false, "items*")
    obj.items.foreach(w.putValue)
    SerializeLog.logPrintf(false, true, false, "items*")

    SerializeLog.logPrintf(true, true, false, "result")
    w.putValue(obj.result)
    SerializeLog.logPrintf(false, true, false, "result")

    SerializeLog.logPrintf(false, true, false, "BlockValue")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val itemsSize = r.getUInt().toIntExact
    val values = (1 to itemsSize).map(_ => r.getValue().asInstanceOf[BlockItem])
    val result = r.getValue()
    cons(values, result)
  }
}
