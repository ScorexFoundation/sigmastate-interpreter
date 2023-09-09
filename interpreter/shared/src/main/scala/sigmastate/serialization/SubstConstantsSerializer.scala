package sigmastate.serialization

import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigmastate.Values.{SValue, Value}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigma.ast.{SCollection, SType}
import sigma.serialization.CoreByteWriter.DataInfo
import sigmastate.SubstConstants

object SubstConstantsSerializer extends ValueSerializer[SubstConstants[SType]] {
  import sigmastate.Operations.SubstConstantsInfo._
  override def opDesc = SubstConstants
  val scriptBytesInfo: DataInfo[SValue] = scriptBytesArg
  val positionsInfo: DataInfo[SValue] = positionsArg
  val newValuesInfo: DataInfo[SValue] = newValuesArg

  def serialize(obj: SubstConstants[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.scriptBytes, scriptBytesInfo)
    w.putValue(obj.positions, positionsInfo)
    w.putValue(obj.newValues, newValuesInfo)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val scriptBytes = r.getValue().asValue[SByteArray]
    val positions = r.getValue().asValue[SIntArray]
    val newVals = r.getValue().asValue[SCollection[SType]]
    SubstConstants(scriptBytes, positions, newVals)
  }
}
