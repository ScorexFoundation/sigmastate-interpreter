package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Consumed, Position}
import sigmastate.serialization.{OpCodes, Serializer, ValueSerializer}
import sigmastate.utxo.ByIndex
import sigmastate.{SCollection, SInt, SType}
import sigmastate.lang.Terms._

object ByIndexSerializer extends ValueSerializer[ByIndex[SType]] {

  override val opCode: OpCode = OpCodes.ByIndexCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ByIndex[SType], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val input = r.getValue().asInstanceOf[Value[SCollection[SType]]]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    val res = ByIndex(input, index, default)
    res -> r.consumed
  }

  override def serializeBody(obj: ByIndex[SType]): Array[Byte] =
    Serializer.startWriter()
      .putValue(obj.input)
      .putValue(obj.index)
      .putOption(obj.default)(_.putValue(_))
      .toBytes

}
