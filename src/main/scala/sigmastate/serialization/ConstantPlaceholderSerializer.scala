package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class ConstantPlaceholderSerializer(cons: (Int, SType) => Value[SType])
  extends ValueSerializer[ConstantPlaceholder[SType]] {

  override val opCode: OpCode = ConstantPlaceholderIndexCode

  override def serializeBody(obj: ConstantPlaceholder[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"Constant placeholder")

    SerializeLog.logPrintf(true, true, false,"id")

    w.putUInt(obj.id)

    SerializeLog.logPrintf(false, true, false,"id")

    SerializeLog.logPrintf(false, true, false,"Constant placeholder")

  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val constant = r.constantStore.get(id)
    if (r.resolvePlaceholdersToConstants)
      constant
    else
      cons(id, constant.tpe)
  }
}

