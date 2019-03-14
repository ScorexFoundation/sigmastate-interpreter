package sigmastate.serialization

import sigmastate.{SCollection, SType}
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utils.SerializeLog

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {

  override val opCode: Byte = ConcreteCollectionCode

  override def serializeBody(cc: ConcreteCollection[_ <: SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ConcreteCollection")

    SerializeLog.logPrintf(true, true, false,"items.size")
    w.putUShort(cc.items.size)
    SerializeLog.logPrintf(false, true, false,"items.size")

    SerializeLog.logPrintf(true, true, false,"tpe.elemType")
    w.putType(cc.tpe.elemType)
    SerializeLog.logPrintf(false, true, false,"tpe.elemType")

    SerializeLog.logPrintf(true, true, false,"items*")
    cc.items.foreach(w.putValue)
    SerializeLog.logPrintf(false, true, false,"items*")

    SerializeLog.logPrintf(false, true, false, "ConcreteCollection")
  }

  override def parseBody(r: SigmaByteReader): Value[SCollection[SType]] = {
    val size = r.getUShort()
    val tItem = r.getType()
    val values =  (1 to size).map(_ => r.getValue())
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value in $values")
    cons(values, tItem)
  }
}
