package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.OptionGetOrElse

case class OptionGetOrElseSerializer(cons: (Value[SOption[SType]], Value[SType]) => Value[SType])
  extends ValueSerializer[OptionGetOrElse[_ <: SType]] {
  import sigmastate.Operations.OptionGetOrElseInfo._
  override def opDesc = OptionGetOrElse

  override def serialize(obj: OptionGetOrElse[_ <: SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisArg)
      .putValue(obj.default, defaultArg)


  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asValue[SOption[SType]]
    val default = r.getValue()
    cons(input, default)
  }
}
