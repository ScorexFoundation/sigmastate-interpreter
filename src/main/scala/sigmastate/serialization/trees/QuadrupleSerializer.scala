package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.{Quadruple, _}
import sigmastate.utils.Extensions._

case class QuadrupleSerializer[S1 <: SType, S2 <: SType, S3 <: SType, S4 <: SType]
(override val opCode: Byte,
 cons: (Value[S1], Value[S2], Value[S3]) => Quadruple[S1, S2, S3, S4])
  extends ValueSerializer[Quadruple[S1, S2, S3, S4]] {

  override def serializeBody(obj: Quadruple[S1, S2, S3, S4], w: ByteWriter): Unit = {
    w.putValue(obj.first)
    w.putValue(obj.second)
    w.putValue(obj.third)
  }

  override def parseBody(r: ByteReader): Quadruple[S1, S2, S3, S4] = {
    val arg1 = r.getValue().asValue[S1]
    val arg2 = r.getValue().asValue[S2]
    val arg3 = r.getValue().asValue[S3]
    cons(arg1, arg2, arg3)
  }
}
