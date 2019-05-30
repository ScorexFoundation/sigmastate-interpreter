package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.GetVar

case class GetVarSerializer(cons: (Byte, SType) => Value[SOption[SType]])
  extends ValueSerializer[GetVar[_ <: SType]] {
  import sigmastate.Operations.GetVarInfo._
  override def opDesc = GetVar

  override def serialize(obj: GetVar[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId, varIdArg)
      .putType(obj.tpe.elemType, ArgInfo("type", "expected type of context variable"))

  override def parse(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
