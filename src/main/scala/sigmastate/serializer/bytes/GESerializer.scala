package sigmastate.serializer.bytes

import sigmastate.{GE, SInt}

object GESerializer {
  val OpCode: Short = 4
}
class GESerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, GE](GESerializer.OpCode, GE)
