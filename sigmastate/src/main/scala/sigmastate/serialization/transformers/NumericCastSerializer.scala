package sigmastate.serialization.transformers

import sigmastate.Values.{Value, SValue}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer

case class NumericCastSerializer(opDesc: NumericCastCompanion,
                                 cons: (Value[SNumericType], SNumericType) => Value[SNumericType])
  extends ValueSerializer[Transformer[SNumericType, SNumericType]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val typeInfo: DataInfo[SType] = ArgInfo("type", "resulting type of the cast operation")

  override def serialize(obj: Transformer[SNumericType, SNumericType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)
      .putType(obj.tpe, typeInfo)

  override def parse(r: SigmaByteReader): Value[SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    cons(input, tpe)
  }
}
