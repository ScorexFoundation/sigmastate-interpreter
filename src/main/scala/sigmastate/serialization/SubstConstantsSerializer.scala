package sigmastate.serialization

import sigmastate.SCollection.{SByteArray, SIntArray}
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{SCollection, SType, SubstConstants}

object SubstConstantsSerializer extends ValueSerializer[SubstConstants[SType]] {
  import sigmastate.Operations.SubstConstantsInfo._
  override def opDesc = SubstConstants

  def serialize(obj: SubstConstants[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.scriptBytes, scriptBytesArg)
    w.putValue(obj.positions, positionsArg)
    w.putValue(obj.newValues, newValuesArg)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val scriptBytes = r.getValue().asValue[SByteArray]
    val positions = r.getValue().asValue[SIntArray]
    val newVals = r.getValue().asValue[SCollection[SType]]
    SubstConstants(scriptBytes, positions, newVals)
  }
}
