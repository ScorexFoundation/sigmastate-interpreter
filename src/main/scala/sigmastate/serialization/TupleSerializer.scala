package sigmastate.serialization

import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position

object TupleSerializer extends ValueSerializer[Tuple] {

  override val opCode: Byte = TupleCode

  override def parseBody(bytes: Array[Byte], pos: Position): (Tuple, Position) = {
    val r = Serializer.startReader(bytes, pos)
    val size = r.getUShort()
    val values =  (1 to size).map(_ => r.getValue())
    (Tuple(values), r.consumed)
  }

  override def serializeBody(tuple: Tuple): Array[Byte] = {
    val length = tuple.length
    require(length <= Short.MaxValue, s"max tuple size is Short.MaxValue = ${Short.MaxValue}")
    val w = Serializer.startWriter()
        .putUShort(length.toShort)
    for (item <- tuple.items) {
      w.putValue(item)
    }
    w.toBytes
  }
}
