package sigmastate.serialization

import sigmastate.SCollection.{SByteArray, SIntArray}
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.{SCollection, SType, SubstConstants}

object SubstConstantsSerializer extends ValueSerializer[SubstConstants[SType]] {

  override val opCode: Byte = OpCodes.SubstConstantsCode

  def serializeBody(obj: SubstConstants[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "SubstConstants")

    SerializeLog.logPrintf(true, true, false, "scriptBytes")
    w.putValue(obj.scriptBytes)
    SerializeLog.logPrintf(false, true, false, "scriptBytes")

    SerializeLog.logPrintf(true, true, false, "positions")
    w.putValue(obj.positions)
    SerializeLog.logPrintf(false, true, false, "positions")

    SerializeLog.logPrintf(true, true, false, "newValues")
    w.putValue(obj.newValues)
    SerializeLog.logPrintf(false, true, false, "newValues")

    SerializeLog.logPrintf(false, true, false, "SubstConstants")
  }

  def parseBody(r: SigmaByteReader): Value[SType] = {
    val scriptBytes = r.getValue().asValue[SByteArray]
    val positions = r.getValue().asValue[SIntArray]
    val newVals = r.getValue().asValue[SCollection[SType]]
    SubstConstants(scriptBytes, positions, newVals)
  }
}
