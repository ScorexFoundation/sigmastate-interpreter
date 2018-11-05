package sigmastate.serialization

import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.SGroupElement
import sigmastate.Values.{SigmaBoolean, Value}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utils.Extensions._

case class ProveDlogSerializer(cons: Value[SGroupElement.type] => SigmaBoolean)
  extends ValueSerializer[ProveDlog] {

  override val opCode: OpCode = OpCodes.ProveDlogCode

  override def serializeBody(obj: ProveDlog, w: ByteWriter): Unit =
    w.putValue(obj.value)

  override def parseBody(r: ByteReader): SigmaBoolean =
    cons(r.getValue().asValue[SGroupElement.type])
}
