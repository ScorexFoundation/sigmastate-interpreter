package sigmastate.serialization

import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}

object TupleSerializer extends ValueSerializer[Tuple] {

  override val opCode: Byte = TupleCode

  override def serializeBody(obj: Tuple, w: ByteWriterSigmaValues): Unit = {
    val length = obj.length
    require(length <= Byte.MaxValue, s"max tuple size is Byte.MaxValue = ${Byte.MaxValue}")
    w.put(length.toByte)
    obj.items.foreach(w.putValue)
  }

  override def parseBody(r: ByteReader): Tuple = {
    val size = r.getByte()
    val values =  (1 to size).map(_ => r.getValue())
    Tuple(values)
  }

}
