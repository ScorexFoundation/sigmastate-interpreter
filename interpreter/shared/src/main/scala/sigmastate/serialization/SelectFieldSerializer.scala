package sigmastate.serialization

import sigmastate.Operations.SelectFieldInfo
import sigma.ast.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SelectField
import SelectFieldInfo._
import sigma.ast.global.SValue
import sigma.ast.{STuple, SType}
import sigma.serialization.CoreByteWriter.DataInfo
import sigmastate.utils.SigmaByteWriter._

case class SelectFieldSerializer(cons: (Value[STuple], Byte) => Value[SType]) extends ValueSerializer[SelectField] {
  override def opDesc = SelectField
  val inputInfo: DataInfo[SValue] = inputArg
  val fieldIndexInfo: DataInfo[Byte] = fieldIndexArg

  override def serialize(obj: SelectField, w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)
      .put(obj.fieldIndex, fieldIndexInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val tuple = r.getValue().asValue[STuple]
    val fieldIndex = r.getByte()
    cons(tuple, fieldIndex)
  }

}
