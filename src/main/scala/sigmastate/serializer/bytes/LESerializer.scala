package sigmastate.serializer.bytes

import sigmastate.{LE, SInt}
import sigmastate.serializer.bytes.base._

object LESerializer {
  private val OpCode: Short = 2
}
class LESerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, LE](LESerializer.OpCode, LE)
