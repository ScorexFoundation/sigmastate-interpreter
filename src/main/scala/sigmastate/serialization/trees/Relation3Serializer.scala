package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}

case class Relation3Serializer[S1 <: SType, S2 <: SType, S3 <: SType, R <: Relation3[S1, S2, S3]]
(override val opCode: Byte,
 cons: (Value[S1], Value[S2], Value[S3]) => R) extends ValueSerializer[R] {

  override def serializeBody(obj: R, w: ByteWriterSigmaValues): Unit = {
    w.putValue(obj.first)
    w.putValue(obj.second)
    w.putValue(obj.third)
  }

  override def parseBody(r: ByteReader): R = {
    val arg1 = r.getValue().asValue[S1]
    val arg2 = r.getValue().asValue[S2]
    val arg3 = r.getValue().asValue[S3]
    cons(arg1, arg2, arg3)
  }
}
