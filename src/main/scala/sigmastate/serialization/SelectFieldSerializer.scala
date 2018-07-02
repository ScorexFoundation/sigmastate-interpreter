package sigmastate.serialization

import sigmastate.STuple
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position
import sigmastate.utxo.SelectField

object SelectFieldSerializer extends ValueSerializer[SelectField] {

  override val opCode: Byte = SelectFieldCode

  override def parseBody(bytes: Array[Byte], pos: Position): (SelectField, Position) = {
    val r = Serializer.startReader(bytes, pos)
    val tuple = r.getValue().asInstanceOf[Value[STuple]]
    val fieldIndex = r.getByte()
    (SelectField(tuple, fieldIndex), r.consumed)
  }

  override def serializeBody(sf: SelectField): Array[Byte] = {
    val w = Serializer.startWriter()
      .putValue(sf.input)
      .put(sf.fieldIndex)
    w.toBytes
  }
}
