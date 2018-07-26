package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

case class TupleSerializer(cons: Seq[Value[SType]] => Value[SType])
  extends ValueSerializer[Tuple] {

  override val opCode: Byte = TupleCode

  override def serializeBody(obj: Tuple, w: ByteWriter): Unit = {
    val length = obj.length
    w.putUByte(length)
    obj.items.foreach(w.putValue)
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val size = r.getByte()
    val values =  (1 to size).map(_ => r.getValue())
    cons(values)
  }

}
