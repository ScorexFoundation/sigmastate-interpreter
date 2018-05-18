package sigmastate.serialization.transformers

import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.SGroupElement
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}

object ProveDiffieHellmanTupleSerializer extends ValueSerializer[ProveDiffieHellmanTuple] {

  override val opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode


  override def serializeBody(obj: ProveDiffieHellmanTuple): Array[Byte] = {
    ValueSerializer.serialize(obj.gv) ++ ValueSerializer.serialize(obj.hv) ++ ValueSerializer.serialize(obj.uv) ++ ValueSerializer.serialize(obj.vv)
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (ProveDiffieHellmanTuple, Consumed) = {
    val (gv, c) = ValueSerializer.deserialize(bytes, pos)
    val (hv, c2) = ValueSerializer.deserialize(bytes, pos + c)
    val (uv, c3) = ValueSerializer.deserialize(bytes, pos + c + c2)
    val (vv, c4) = ValueSerializer.deserialize(bytes, pos + c + c2 + c3)
    val tuple = ProveDiffieHellmanTuple(gv.asInstanceOf[Value[SGroupElement.type]],
      hv.asInstanceOf[Value[SGroupElement.type]],
      uv.asInstanceOf[Value[SGroupElement.type]],
      vv.asInstanceOf[Value[SGroupElement.type]])

    (tuple, c + c2 + c3 + c4)
  }

}
