package sigmastate.serializer.bytes

import sigmastate.{LE, SInt}

object LESerializer {
  val OpCode: Short = 2
}
class LESerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, LE](LESerializer.OpCode, LE)
