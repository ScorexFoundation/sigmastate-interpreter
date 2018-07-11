package sigmastate.serialization

import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.SGroupElement
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{ByteReaderSigmaValues, ByteWriterSigmaValues}

object ProveDlogSerializer extends ValueSerializer[ProveDlog] {

  override val opCode: OpCode = OpCodes.ProveDlogCode

  override def serializeBody(obj: ProveDlog, w: ByteWriterSigmaValues): Unit =
    w.putValue(obj.value)

  override def parseBody(r: ByteReaderSigmaValues): ProveDlog =
    ProveDlog(r.getValue().asValue[SGroupElement.type])
}
