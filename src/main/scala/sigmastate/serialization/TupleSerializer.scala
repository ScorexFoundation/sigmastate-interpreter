package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class TupleSerializer(cons: Seq[Value[SType]] => Value[SType])
  extends ValueSerializer[Tuple] {

  override val opCode: Byte = TupleCode

  override def serializeBody(obj: Tuple, w: SigmaByteWriter): Unit = {
    val length = obj.length
    SerializeLog.logPrintf(true, true, false,"Tuple")

    SerializeLog.logPrintf(true, true, false,"length")
    w.putUByte(length)
    SerializeLog.logPrintf(false, true, false,"length")

    SerializeLog.logPrintf(true, true, false,"items*")
    obj.items.foreach(w.putValue)
    SerializeLog.logPrintf(false, true, false,"items*")

    SerializeLog.logPrintf(false, true, false,"Tuple")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val size = r.getByte()
    val values =  (1 to size).map(_ => r.getValue())
    cons(values)
  }

}
