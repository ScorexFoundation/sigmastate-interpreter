package sigmastate.serialization.transformers

import sigmastate.{SGroupElement, CreateProveDHTuple}
import sigmastate.Values.{Value, SigmaPropValue}
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.serialization._

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

case class CreateProveDHTupleSerializer(cons: (Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type]) => SigmaPropValue)
    extends ValueSerializer[CreateProveDHTuple] {

  override val opCode: OpCode = OpCodes.ProveDHTupleCode

  override def serialize(obj: CreateProveDHTuple, w: SigmaByteWriter): Unit = {
    w.putValue(obj.gv)
    w.putValue(obj.hv)
    w.putValue(obj.uv)
    w.putValue(obj.vv)
  }

  override def parse(r: SigmaByteReader) = {
    val gv = r.getValue().asValue[SGroupElement.type]
    val hv = r.getValue().asValue[SGroupElement.type]
    val uv = r.getValue().asValue[SGroupElement.type]
    val vv = r.getValue().asValue[SGroupElement.type]
    cons(gv, hv, uv, vv)
  }
}
