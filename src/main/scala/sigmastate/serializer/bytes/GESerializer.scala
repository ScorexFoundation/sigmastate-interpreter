package sigmastate.serializer.bytes

import sigmastate.{GE, SInt}
import sigmastate.serializer.bytes.base._

object GESerializer {
  private val OpCode: Short = 4
}
class GESerializer extends TwoOperandOperationSerializer[SInt.type, SInt.type, GE](GESerializer.OpCode, GE)
