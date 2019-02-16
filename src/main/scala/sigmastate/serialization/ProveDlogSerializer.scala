package sigmastate.serialization

import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.{SGroupElement, CreateProveDlog}
import sigmastate.Values.{Value, SigmaBoolean, SigmaPropValue}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ProveDlogSerializer(cons: EcPointType => ProveDlog)
  extends Serializer[ProveDlog, ProveDlog] {

  override def serializeBody(obj: ProveDlog, w: SigmaByteWriter): Unit =
    DataSerializer.serialize[SGroupElement.type](obj.value, SGroupElement, w)

  override def parseBody(r: SigmaByteReader) = {
    val res = DataSerializer.deserialize(SGroupElement, r)
    cons(res)
  }
}

case class CreateProveDlogSerializer(cons: Value[SGroupElement.type] => SigmaPropValue)
    extends ValueSerializer[CreateProveDlog] {
  override val opCode: OpCode = OpCodes.ProveDlogCode

  override def serializeBody(obj: CreateProveDlog, w: SigmaByteWriter): Unit = {
    w.putValue(obj.value)
  }

  override def parseBody(r: SigmaByteReader) = {
    val v = r.getValue().asValue[SGroupElement.type]
    cons(v)
  }
}
