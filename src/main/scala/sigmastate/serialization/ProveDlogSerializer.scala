package sigmastate.serialization

import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.SGroupElement
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{ByteReader, ByteWriter}

object ProveDlogSerializer extends ValueSerializer[ProveDlog] {

  override val opCode: OpCode = OpCodes.ProveDlogCode

  override def serializeBody(obj: ProveDlog, w: ByteWriter): Unit =
    w.putValue(obj.value)

  override def parseBody(r: ByteReader): ProveDlog =
    ProveDlog(r.getValue().asValue[SGroupElement.type])
}
