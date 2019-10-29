package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.GetVar

case class GetVarSerializer(cons: (Byte, SType) => Value[SOption[SType]])
  extends ValueSerializer[GetVar[_ <: SType]] {
  import sigmastate.Operations.GetVarInfo._
  override def opDesc = GetVar
  val typeInfo: DataInfo[SType] = ArgInfo("type", "expected type of context variable")

  override def serialize(obj: GetVar[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId, varIdArg)
      .putType(obj.tpe.elemType, typeInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
