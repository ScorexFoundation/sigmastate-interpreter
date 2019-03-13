package sigmastate.serialization

import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.SGroupElement
import sigmastate.Values.{SigmaBoolean, Value}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}


case class ProveDlogSerializer(cons: Value[SGroupElement.type] => SigmaBoolean)
  extends ValueSerializer[ProveDlog] {

  override val opCode: OpCode = OpCodes.ProveDlogCode

  override def serializeBody(obj: ProveDlog, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"ProveDlog")

    w.putValue(obj.value)

    SerializeLog.logPrintf(false, true, false,"ProveDlog")
  }

  override def parseBody(r: SigmaByteReader): SigmaBoolean =
    cons(r.getValue().asValue[SGroupElement.type])
}
