package sigmastate.serialization

import com.google.common.primitives.Ints
import sigmastate.Values.BoxConstant
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.utxo.ErgoBox

object BoxConstantSerializer extends ValueSerializer[BoxConstant] {
  override val opCode: OpCode = OpCodes.BoxConstantCode

  val IntLength = Ints.BYTES

  override def parseBody(bytes: Array[Byte], pos: Position): (BoxConstant, Consumed) = {
    val length = Ints.fromByteArray(bytes.slice(pos, pos + IntLength))
    val box = ErgoBox.serializer.parseBytes(bytes.slice(pos + IntLength, pos + IntLength + length)).get
    BoxConstant(box) -> (IntLength + length)
  }

  override def serializeBody(obj: BoxConstant): Array[Byte] = {
    val bytes = ErgoBox.serializer.toBytes(obj.value)
    val length = bytes.length
    Ints.toByteArray(length) ++ bytes
  }
}
