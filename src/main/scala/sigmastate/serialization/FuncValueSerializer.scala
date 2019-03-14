package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

case class FuncValueSerializer(cons: (IndexedSeq[(Int, SType)], Value[SType]) => Value[SType])
  extends ValueSerializer[FuncValue] {

  override val opCode: OpCode = FuncValueCode

  override def serializeBody(obj: FuncValue, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "FuncValue")

    SerializeLog.logPrintf(true, true, false, "args.length")
    w.putUInt(obj.args.length)
    SerializeLog.logPrintf(false, true, false, "args.length")

    SerializeLog.logPrintf(true, true, false, "(args.idx, args.tpe)*")
    obj.args.foreach{ case (idx, tpe) => w.putUInt(idx).putType(tpe) }
    SerializeLog.logPrintf(false, true, false, "(args.idx, args.tpe)*")

    SerializeLog.logPrintf(true, true, false, "body")
    w.putValue(obj.body)
    SerializeLog.logPrintf(false, true, false, "body")

    SerializeLog.logPrintf(false, true, false, "FuncValue")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val argsSize = r.getUInt().toIntExact
    val argsBuilder = mutable.ArrayBuilder.make[(Int, SType)]()
    for (_ <- 0 until argsSize) {
      val id = r.getUInt().toInt
      val tpe = r.getType()
      r.valDefTypeStore(id) = tpe
      argsBuilder += ((id, tpe))
    }
    val body = r.getValue()
    cons(argsBuilder.result(), body)
  }
}
