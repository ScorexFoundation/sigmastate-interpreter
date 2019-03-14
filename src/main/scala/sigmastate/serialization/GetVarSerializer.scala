package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.GetVar

case class GetVarSerializer(cons: (Byte, SType) => Value[SOption[SType]])
  extends ValueSerializer[GetVar[_ <: SType]] {

  override val opCode: OpCode = GetVarCode

  override def serializeBody(obj: GetVar[_ <: SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "GetVar")

    SerializeLog.logPrintf(true, true, false, "varId")
    w.put(obj.varId)
    SerializeLog.logPrintf(false, true, false, "varId")

    SerializeLog.logPrintf(true, true, false, "tpe.elemType")
    w.putType(obj.tpe.elemType)
    SerializeLog.logPrintf(false, true, false, "tpe.elemType")

    SerializeLog.logPrintf(false, true, false, "GetVar")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
