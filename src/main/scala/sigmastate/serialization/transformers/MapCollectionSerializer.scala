package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.MapCollection
import sigmastate.{SCollection, SFunc, SType}

case class MapCollectionSerializer(cons: (Value[SCollection[SType]], Value[SFunc]) => Value[SType])
  extends ValueSerializer[MapCollection[SType, SType]] {

  override val opCode: OpCode = OpCodes.MapCollectionCode

  override def serializeBody(obj: MapCollection[SType, SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "MapCollection")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "mapper")
    w.putValue(obj.mapper)
    SerializeLog.logPrintf(false, true, false, "mapper")

    SerializeLog.logPrintf(false, true, false, "MapCollection")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asValue[SCollection[SType]]
    val mapper = r.getValue().asFunc
    cons(input, mapper)
  }

}
