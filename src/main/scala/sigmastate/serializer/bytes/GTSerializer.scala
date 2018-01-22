package sigmastate.serializer.bytes

import sigmastate.{GT, SInt}
import sigmastate.serializer.bytes.base._

object GTSerializer {
  private val OpCode: Short = 3
}
class GTSerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, GT](GTSerializer.OpCode, GT)
