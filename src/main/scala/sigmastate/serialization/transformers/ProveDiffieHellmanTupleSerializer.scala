package sigmastate.serialization.transformers

import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.SGroupElement
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReaderSigmaValues, ByteWriterSigmaValues}

object ProveDiffieHellmanTupleSerializer extends ValueSerializer[ProveDiffieHellmanTuple] {

  override val opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode

  override def serializeBody(obj: ProveDiffieHellmanTuple, w: ByteWriterSigmaValues): Unit = {
    w.putValue(obj.gv)
    w.putValue(obj.hv)
    w.putValue(obj.uv)
    w.putValue(obj.vv)
  }

  override def parseBody(r: ByteReaderSigmaValues): ProveDiffieHellmanTuple = {
    val gv = r.getValue().asValue[SGroupElement.type]
    val hv = r.getValue().asValue[SGroupElement.type]
    val uv = r.getValue().asValue[SGroupElement.type]
    val vv = r.getValue().asValue[SGroupElement.type]
    ProveDiffieHellmanTuple(gv, hv, uv, vv)
  }
}
