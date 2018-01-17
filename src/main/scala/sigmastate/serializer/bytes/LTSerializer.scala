package sigmastate.serializer.bytes

import sigmastate.{LT, SInt}

object LTSerializer {
  val OpCode: Short = 1
}
class LTSerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, LT](LTSerializer.OpCode, LT)
