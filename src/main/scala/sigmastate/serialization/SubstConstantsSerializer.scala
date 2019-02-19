package sigmastate.serialization

import sigmastate.SCollection.{SByteArray, SIntArray}
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{SCollection, SType, SubstConstants}

object SubstConstantsSerializer extends ValueSerializer[SubstConstants[SType]] {

  override val opCode: Byte = OpCodes.SubstConstantsCode

  def serialize(obj: SubstConstants[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.scriptBytes)
    w.putValue(obj.positions)
    w.putValue(obj.newValues)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val scriptBytes = r.getValue().asValue[SByteArray]
    val positions = r.getValue().asValue[SIntArray]
    val newVals = r.getValue().asValue[SCollection[SType]]
    SubstConstants(scriptBytes, positions, newVals)
  }
}
