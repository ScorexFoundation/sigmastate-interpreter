package sigmastate.serialization

import sigma.crypto.EcPointType
import sigma.serialization.GroupElementSerializer
import sigmastate.ProveDlog
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ProveDlogSerializer(cons: EcPointType => ProveDlog)
  extends SigmaSerializer[ProveDlog, ProveDlog] {

  override def serialize(obj: ProveDlog, w: SigmaByteWriter): Unit =
    GroupElementSerializer.serialize(obj.value, w)

  override def parse(r: SigmaByteReader) = {
    val res = GroupElementSerializer.parse(r)
    cons(res)
  }
}




