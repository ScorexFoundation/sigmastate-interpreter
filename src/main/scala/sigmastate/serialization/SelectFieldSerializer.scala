package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SelectField
import sigmastate.{STuple, SType}

case class SelectFieldSerializer(cons: (Value[STuple], Byte) => Value[SType]) extends ValueSerializer[SelectField] {

  override val opCode: Byte = SelectFieldCode

  override def serializeBody(obj: SelectField, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"SelectField")

    SerializeLog.logPrintf(true, true, false,"input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false,"input")

    SerializeLog.logPrintf(true, true, false,"fieldIndex")
    w.put(obj.fieldIndex)
    SerializeLog.logPrintf(false, true, false,"fieldIndex")

    SerializeLog.logPrintf(false, true, false,"SelectField")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val tuple = r.getValue().asValue[STuple]
    val fieldIndex = r.getByte()
    cons(tuple, fieldIndex)
  }

}
