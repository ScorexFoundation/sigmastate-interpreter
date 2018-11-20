package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SelectField
import sigmastate.{STuple, SType}

case class SelectFieldSerializer(cons: (Value[STuple], Byte) => Value[SType]) extends ValueSerializer[SelectField] {

  override val opCode: Byte = SelectFieldCode

  override def serialize(obj: SelectField, w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .put(obj.fieldIndex)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val tuple = r.getValue().asValue[STuple]
    val fieldIndex = r.getByte()
    cons(tuple, fieldIndex)
  }

}
