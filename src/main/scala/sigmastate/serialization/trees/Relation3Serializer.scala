package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import scorex.util.Extensions._

case class Relation3Serializer[S1 <: SType, S2 <: SType, S3 <: SType, R <: Value[SBoolean.type]]
(override val opCode: Byte,
 cons: (Value[S1], Value[S2], Value[S3]) => R) extends ValueSerializer[R] {

  override def serialize(obj: R, w: SigmaByteWriter): Unit = {
    val rel = obj.asInstanceOf[Relation3[S1, S2, S3]]
    w.putValue(rel.first)
    w.putValue(rel.second)
    w.putValue(rel.third)
  }

  override def parse(r: SigmaByteReader): R = {
    val arg1 = r.getValue().asValue[S1]
    val arg2 = r.getValue().asValue[S2]
    val arg3 = r.getValue().asValue[S3]
    cons(arg1, arg2, arg3)
  }
}
