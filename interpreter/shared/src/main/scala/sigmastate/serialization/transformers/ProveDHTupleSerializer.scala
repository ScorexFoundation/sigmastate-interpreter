package sigmastate.serialization.transformers

import sigma.crypto.EcPointType
import sigma.serialization.GroupElementSerializer
import sigmastate.ProveDHTuple
import sigmastate.serialization._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ProveDHTupleSerializer(
     cons: (EcPointType, EcPointType, EcPointType, EcPointType) => ProveDHTuple
  ) extends SigmaSerializer[ProveDHTuple, ProveDHTuple] {

  override def serialize(obj: ProveDHTuple, w: SigmaByteWriter): Unit = {
    GroupElementSerializer.serialize(obj.gv, w)
    GroupElementSerializer.serialize(obj.hv, w)
    GroupElementSerializer.serialize(obj.uv, w)
    GroupElementSerializer.serialize(obj.vv, w)
  }

  override def parse(r: SigmaByteReader) = {
    val gv = GroupElementSerializer.parse(r)
    val hv = GroupElementSerializer.parse(r)
    val uv = GroupElementSerializer.parse(r)
    val vv = GroupElementSerializer.parse(r)
    cons(gv, hv, uv, vv)
  }
}

