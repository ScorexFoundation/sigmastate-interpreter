package sigmastate.serialization.transformers

import sigmastate.{SGroupElement, CreateProveDHTuple}
import sigmastate.Values.{Value, SigmaPropValue}
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.serialization.{ValueSerializer, DataSerializer, OpCodes, SigmaSerializer}

case class ProveDHTupleSerializer(
     cons: (EcPointType, EcPointType, EcPointType, EcPointType) => ProveDHTuple
  ) extends SigmaSerializer[ProveDHTuple, ProveDHTuple] {

  override def serialize(obj: ProveDHTuple, w: SigmaByteWriter): Unit = {
    DataSerializer.serialize[SGroupElement.type](obj.gv, SGroupElement, w)
    DataSerializer.serialize[SGroupElement.type](obj.hv, SGroupElement, w)
    DataSerializer.serialize[SGroupElement.type](obj.uv, SGroupElement, w)
    DataSerializer.serialize[SGroupElement.type](obj.vv, SGroupElement, w)
  }

  override def parse(r: SigmaByteReader) = {
    val gv = DataSerializer.deserialize(SGroupElement, r)
    val hv = DataSerializer.deserialize(SGroupElement, r)
    val uv = DataSerializer.deserialize(SGroupElement, r)
    val vv = DataSerializer.deserialize(SGroupElement, r)
    cons(gv, hv, uv, vv)
  }
}

case class CreateProveDHTupleSerializer(cons: (Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type]) => SigmaPropValue)
    extends ValueSerializer[CreateProveDHTuple] {

  override val opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode

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
