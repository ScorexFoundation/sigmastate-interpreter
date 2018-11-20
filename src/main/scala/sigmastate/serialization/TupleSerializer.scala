package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class TupleSerializer(cons: Seq[Value[SType]] => Value[SType])
  extends ValueSerializer[Tuple] {

  override val opCode: Byte = TupleCode

  override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = {
    val length = obj.length
    w.putUByte(length)
    obj.items.foreach(w.putValue)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val size = r.getByte()
    val values =  (1 to size).map(_ => r.getValue())
    cons(values)
  }

}
