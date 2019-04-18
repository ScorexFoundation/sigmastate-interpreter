package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._

case class TupleSerializer(cons: Seq[Value[SType]] => Value[SType])
  extends ValueSerializer[Tuple] {
  override def opDesc = Tuple

  override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = {
    val length = obj.length
    w.putUByte(length, "numItems")
    foreach("items", obj.items) { i => w.putValue(i, "item") }
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val size = r.getByte()
    val values =  (1 to size).map(_ => r.getValue())
    cons(values)
  }

}
