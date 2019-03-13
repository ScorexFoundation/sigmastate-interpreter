package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.{Quadruple, _}

case class QuadrupleSerializer[S1 <: SType, S2 <: SType, S3 <: SType, S4 <: SType]
(override val opCode: Byte,
 cons: (Value[S1], Value[S2], Value[S3]) => Value[S4])
  extends ValueSerializer[Quadruple[S1, S2, S3, S4]] {

  override def serializeBody(obj: Quadruple[S1, S2, S3, S4], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"Quadruple")

    SerializeLog.logPrintf(true, true, false,"First")
    w.putValue(obj.first)
    SerializeLog.logPrintf(false, true, false,"First")

    SerializeLog.logPrintf(true, true, false,"Second")
    w.putValue(obj.second)
    SerializeLog.logPrintf(false, true, false,"Second")

    SerializeLog.logPrintf(true, true, false,"Third")
    w.putValue(obj.third)
    SerializeLog.logPrintf(false, true, false,"Third")

    SerializeLog.logPrintf(false, true, false,"Quadruple")
  }

  override def parseBody(r: SigmaByteReader): Value[S4] = {
    val arg1 = r.getValue().asValue[S1]
    val arg2 = r.getValue().asValue[S2]
    val arg3 = r.getValue().asValue[S3]
    cons(arg1, arg2, arg3)
  }
}
