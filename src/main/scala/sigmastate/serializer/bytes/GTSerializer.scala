package sigmastate.serializer.bytes

import sigmastate.{GT, SInt}

object GTSerializer {
  val OpCode: Short = 3
}
class GTSerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, GT](GTSerializer.OpCode, GT)
