package sigmastate.serialization

import sigmastate.STuple
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}
import sigmastate.utxo.SelectField

object SelectFieldSerializer extends ValueSerializer[SelectField] {

  override val opCode: Byte = SelectFieldCode

  override def serializeBody(obj: SelectField, w: ByteWriterSigmaValues): Unit =
    w.putValue(obj.input)
      .put(obj.fieldIndex)

  override def parseBody(r: ByteReader): SelectField = {
    val tuple = r.getValue().asValue[STuple]
    val fieldIndex = r.getByte()
    SelectField(tuple, fieldIndex)
  }

}
