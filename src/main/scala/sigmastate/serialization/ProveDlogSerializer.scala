package sigmastate.serialization

import com.google.common.primitives.Ints
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.SGroupElement
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer._

object ProveDlogSerializer extends ValueSerializer[ProveDlog] {

  val INT_LENGTH = Ints.BYTES

  override val opCode: OpCode = OpCodes.ProveDlogCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ProveDlog, Consumed) = {
    val (ge, consumed) = ValueSerializer.deserialize(bytes, pos)
    ProveDlog(ge.asInstanceOf[Value[SGroupElement.type]]) -> consumed
  }

  override def serializeBody(obj: ProveDlog): Array[Byte] = {
    ValueSerializer.serialize(obj.value)
  }
}
