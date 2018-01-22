package sigmastate.serializer.bytes

import scorex.core.serialization.Serializer
import sigmastate.{EQ, SType, Value}

object EQSerializer {
  val OpCode: Short = 5
}
class EQSerializer[LT <: SType, RT <: SType](implicit leftSerializer: Serializer[Value[LT]],
                                             rightSerializer: Serializer[Value[RT]]) extends TwoOperandOperationSerializer[LT, RT, EQ[LT, RT]](EQSerializer.OpCode, EQ.apply)
