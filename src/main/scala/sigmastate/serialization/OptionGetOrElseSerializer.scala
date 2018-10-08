package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.OptionGetOrElse

case class OptionGetOrElseSerializer(cons: (Value[SOption[SType]], Value[SType]) => Value[SType])
  extends ValueSerializer[OptionGetOrElse[_ <: SType]] {

  override val opCode: OpCode = OptionGetOrElseCode

  override def serializeBody(obj: OptionGetOrElse[_ <: SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.default)


  override def parseBody(r: ByteReader): Value[SType] = {
    val input = r.getValue().asValue[SOption[SType]]
    val default = r.getValue()
    cons(input, default)
  }
}
