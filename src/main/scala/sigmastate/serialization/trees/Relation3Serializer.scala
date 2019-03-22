package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigma.util.Extensions._

case class Relation3Serializer[S1 <: SType, S2 <: SType, S3 <: SType, R <: Value[SBoolean.type]]
(override val opCode: Byte,
 cons: (Value[S1], Value[S2], Value[S3]) => R) extends ValueSerializer[R] {

  override def serializeBody(obj: R, w: SigmaByteWriter): Unit = {
    val rel = obj.asInstanceOf[Relation3[S1, S2, S3]]
    SerializeLog.logPrintf(true, true, false,"Relation3")

    SerializeLog.logPrintf(true, true, false,"first")
    w.putValue(rel.first)
    SerializeLog.logPrintf(false, true, false,"first")

    SerializeLog.logPrintf(true, true, false,"second")
    w.putValue(rel.second)
    SerializeLog.logPrintf(false, true, false,"second")

    SerializeLog.logPrintf(true, true, false,"third")
    w.putValue(rel.third)
    SerializeLog.logPrintf(false, true, false,"third")

    SerializeLog.logPrintf(false, true, false,"Relation3")
  }

  override def parseBody(r: SigmaByteReader): R = {
    val arg1 = r.getValue().asValue[S1]
    val arg2 = r.getValue().asValue[S2]
    val arg3 = r.getValue().asValue[S3]
    cons(arg1, arg2, arg3)
  }
}
