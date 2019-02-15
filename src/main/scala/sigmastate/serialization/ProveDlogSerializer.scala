package sigmastate.serialization

import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.SGroupElement
import sigmastate.Values.{Value, SigmaBoolean}
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ProveDlogSerializer(cons: Value[SGroupElement.type] => SigmaBoolean)
  extends Serializer[ProveDlog, ProveDlog] {

  override def serializeBody(obj: ProveDlog, w: SigmaByteWriter): Unit =
    w.putValue(obj.value)

  override def parseBody(r: SigmaByteReader): ProveDlog =
    cons(r.getValue().asValue[SGroupElement.type]).asInstanceOf[ProveDlog]
}

