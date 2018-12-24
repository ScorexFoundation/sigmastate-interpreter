package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.GetVar

case class GetVarSerializer(cons: (Byte, SType) => Value[SOption[SType]])
  extends ValueSerializer[GetVar[_ <: SType]] {

  override val opCode: OpCode = GetVarCode

  override def serializeBody(obj: GetVar[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId)
      .putType(obj.tpe.elemType)

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
